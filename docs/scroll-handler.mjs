const elementPositionMap = new Map();
const DEBOUNCE_TIMEOUT = 100;
let debounceTimer = null;

window.addEventListener("click", (event) => {
  const { target: { hash } } = event;
  if (!hash) return;
  const element = document.querySelector(hash);
  element.scrollIntoView({
    behavior: "smooth",
  });
  event.preventDefault();
});

window.addEventListener("scroll", debounceScroll, { passive: true });
handleScroll();

const resizeObserver = new ResizeObserver(handleResize);
resizeObserver.observe(document.body);

function handleResize() {
  for (const element of document.querySelectorAll("main h2[id]")) {
    let { top, height } = element.getBoundingClientRect();
    top += window.scrollY;
    elementPositionMap.set(element, { top, height });
  }
}

function debounceScroll() {
  clearTimeout(debounceTimer);
  debounceTimer = setTimeout(() => {
    handleScroll();
  }, DEBOUNCE_TIMEOUT);
}

function handleScroll() {
  const targetY = window.scrollY;

  if (targetY < 200) {
    window.history.replaceState(null, "", window.location.pathname);
    return;
  }

  let minDistance;
  let targetURL, targetElement;
  for (const [element, rect] of elementPositionMap) {
    const elementY = rect.top + rect.height / 2;
    const distance = Math.abs(elementY - targetY);
    if (!minDistance || distance < minDistance) {
      targetURL = `${window.location.pathname}#${element.id}`;
      targetElement = element;
      minDistance = distance;
    }
  }
  if (targetElement && window.location.hash !== `#${targetElement.id}`) {
    window.history.replaceState(null, "", targetURL);
  }
}
