import s2 from "../dist/main.js";
import depCheck from "../dist/dep-check.js";

s2.debug = true;
depCheck();
cleanupTemplates();

const template = document.querySelector("#root");

const [node, proxy] = s2({
  title: "Hello, <em>world!</em>",
  titleClass: "title",
  titleLang: "en",
  list: {
    things: [{ text: 'a' }, { text: 'b' }, { text: 'c' }],
    'try': [
        { run, code: `p.list.things.reverse()` },
        { run, code: `p.list.things.push({ text: 'd' })` },
        { run, code: `p.list.things.unshift({ text: 'z' })` },
        { run, code: `p.list.things.splice(1, 1, { text: 'x' }, { text: 'y' })` },
        { run, code: `p.list.things.sort((a, b) => a.text < b.text)` },
        { run, code: `p.list.things.length = 2` },
        { run, code: `delete p.list.things` },
        { run, code: `p.list.things = [ { text: 'e' }, { text: 'f' } ]` },
    ],
  },
  counter: {
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
