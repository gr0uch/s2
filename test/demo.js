import s2 from "../dist/main.js";

cleanupTemplates();

const template = document.getElementById("menu");
const [node, proxy] = s2({
  title: "Hello, <em>world!</em>",
  easterEgg() {
    console.log("henlo", this);
  },
  counter: {
    label: {
      text: 'c',
    },
    count: 0,
    increment() {
      this.count++;
    },
  },
}, template);
document.body.appendChild(node);

window.p = proxy;

function cleanupTemplates() {
  document.querySelectorAll('template').forEach(template => {
    const { content } = template;
    const SHOW_TEXT = 0b000000000100;
    const iter = document.createNodeIterator(content, SHOW_TEXT);
    let node;
    while (node = iter.nextNode()) {
      if (!node.nodeValue.trim().length)
        node.remove();
    }
  });
}
