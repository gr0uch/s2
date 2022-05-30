import s2, { mount, move } from "./main.mjs";

const demoContainer = document.querySelector(".demo-container");
const boxes = demoContainer.querySelector(".boxes");

let isVisible;

const obj = {
  boxes: boxes.textContent.split("").map(char => {
    return char.trim() ? ({ char, [move]: animate }) : null;
  }).filter(Boolean),
  [mount](node) {
    const obs = new IntersectionObserver((entries) => {
      for (const { isIntersecting } of entries) {
        isVisible = isIntersecting;
        if (isIntersecting) mutate();
      }
    });
    obs.observe(node);
  }
};

const box = boxes.firstElementChild;
box.dataset.key = "boxes";
box.innerHTML = "";
const text = document.createElement("span");
text.dataset.text = "char";
box.appendChild(text);
boxes.innerHTML = "";
boxes.appendChild(box);

const [proxy, fragment] = s2(obj, demoContainer);

demoContainer.parentNode.insertBefore(fragment, demoContainer);
demoContainer.remove();

function mutate() {
  proxy.boxes.sort((a, b) => {
    return Math.random() - 0.2;
  });
  setTimeout(() => {
    if (!isVisible) return;
    mutate();
  }, 750);
}

function animate(node) {
  if (this.isMoving ||
    node.nodeType !== 1) return;
  this.isMoving = true;
  const b1 =
    node.getBoundingClientRect();
  requestAnimationFrame(() => {
    delete this.isMoving;
    const b2 =
      node.getBoundingClientRect();
    const i = b1.left - b2.left;
    const j = b1.top - b2.top;
    const t1 = `translateX(${i}px)
                translateY(${j}px)`;
    const t2 = `translateX(0)
                translateY(0)`;
    const c1 = "var(--c-e)";
    const c2 = "var(--c-a2)";
    node.animate([
      { transform: t1, color: c1 },
      { transform: t2, color: c2 },
    ], {
      duration: 500,
      easing: 'ease-in-out',
    });
  });
}
