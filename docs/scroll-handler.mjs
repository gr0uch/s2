const elementPositionMap = new Map();
const THROTTLE_TIMEOUT = 100;
let throttleTimer = null;

window.addEventListener("click", (event) => {
  const { target: { hash } } = event;
  if (!hash) return;
  const element = document.querySelector(hash);
  element.scrollIntoView({
    behavior: "smooth",
  });
  event.preventDefault();
});

window.addEventListener("scroll", handleScroll, { passive: true });
const resizeObserver = new ResizeObserver(handleResize);
resizeObserver.observe(document.body);
handleScroll();

function handleResize() {
  for (const element of document.querySelectorAll("main h2[id]")) {
    let { top, height } = element.getBoundingClientRect();
    top += window.scrollY;
    elementPositionMap.set(element, { top, height });
  }
}

function handleScroll() {
  if (throttleTimer) return;
  throttleTimer = setTimeout(() => {
    clearTimeout(throttleTimer);
    throttleTimer = null;
  }, THROTTLE_TIMEOUT);

  const targetY = window.scrollY;

  if (targetY === 0) {
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
