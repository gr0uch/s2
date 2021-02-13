import s2 from "../dist/main.js";

s2.debug = true;
cleanupTemplates();

const template = document.getElementById("menu");

const [node, proxy] = s2({
  title: "Hello, <em>world!</em>",
  easterEgg() {
    this.title = "Fak!";
  },
  counter: {
    things: [{ text: 'a' }, { text: 'b' }, { text: 'c' }],
    'try': [
        { run, code: `p.counter.things.reverse()` },
        { run, code: `p.counter.things.push({ text: 'd' })` },
        { run, code: `p.counter.things.unshift({ text: 'z' })` },
        { run, code: `p.counter.things.splice(1, 1, { text: 'x' }, { text: 'y' })` },
        { run, code: `p.counter.things.sort((a, b) => a.text < b.text)` },
        { run, code: `p.counter.things.length = 2` },
    ],
    count: 0,
    increment() {
      this.count++;
    },
  },
}, template);

document.body.appendChild(node);

window.p = proxy;

function run() {
  eval(this.code);
}

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
