import s2, { mount, unmount } from "../dist/main.js";
import depCheck from "../dist/dep-check.js";
import test from "./runner.js";

const start = performance.now();
const initialThings = [
  { text: 'a' },
  { text: 'b' },
  { text: 'c' },
];

test.initialThings = initialThings;
s2.debug = true;
depCheck();
cleanupTemplates();

const template = document.querySelector("#root");

const [proxy, node] = s2({
  title: "Hello, <em>world!</em>",
  titleClass: "title",
  titleLang: "en",
  list: {
    things: initialThings.slice(),
    'try': [
        { run, code: `p.list.things.reverse()` },
        { run, code: `p.list.things.push({ text: 'd' })` },
        { run, code: `p.list.things.unshift({ text: 'z' })` },
        { run, code: `p.list.things.splice(1, 1, { text: 'x' }, { text: 'y' })` },
        { run, code: `p.list.things.sort(({ text: a }, { text: b }) => b.localeCompare(a))` },
        { run, code: `p.list.things.length = 2` },
        { run, code: `delete p.list.things` },
        { run, code: `p.list.things = [ { text: 'e' }, { text: 'f' } ]` },
    ],
  },
  counter: {
    count: 0,
    total: 0,
    runText: 'Run test',
    test,
    [mount]: function(node) {
      console.log('Mount called', node, this);
    },
    [unmount]: delayUnmount,
  },
  f: new Array(1000).fill().map(() => ({ f: 'f' })),
  [mount]: function spam(node) {
    return null;
    const t = {};
    let i, n, c;
    n = document.createElement('span')
    n.textContent = 'd';
    for (i = 0; i < 1000; i++) {
      c = n.cloneNode(true);
      node.appendChild(document.createComment('fak'));
      node.appendChild(c);
      node.appendChild(document.createComment('u'));
    }
  },
}, template);

document.body.appendChild(node);
console.log(`Mounted in ${performance.now() - start} ms`);

window.p = proxy;

async function delayUnmount(node) {
  console.log('Unmount called', node, this);
  node.style.color = '#f35';
  await new Promise(resolve => setTimeout(resolve, 500));
}

function run() {
  eval(this.code);
}

function cleanupTemplates() {
  document.querySelectorAll('template').forEach(template => {
    const iter = document
      .createNodeIterator(template.content, NodeFilter.SHOW_TEXT);
    let node;
    while (node = iter.nextNode()) {
      if (!node.nodeValue.trim().length)
        node.remove();
    }
  });
}
