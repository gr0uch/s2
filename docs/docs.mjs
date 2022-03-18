import s2, { root, target, mount, unmount, move, registerTemplate } from "./main.mjs";
import { observable, createComputed } from "./computed-properties.mjs";
import parseMustache from "./mustache.mjs";

// s2.debug = true;

const computed = createComputed(mount, unmount);
let i = 0;

// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AsyncFunction
const AsyncFunction = Object.getPrototypeOf(async function() {}).constructor;

for (const entry of Array.prototype.reduce.call(
  document.querySelectorAll(".live-example.code"),
  (list, element) => {
    list.push(processExample(element));
    element.addEventListener("input", () => {
      executeExample(processExample(element));
    });
    return list;
  },
  [],
)) {
  try {
    executeExample(entry);
  } catch (e) {
    console.error(e);
  }
  i++;
}

console.info(`${i} examples processed`);

function processExample(element) {
  const [{ value: js }, { value: html }] = element.querySelectorAll("textarea");
  const output = Array.prototype.find.call(
    document.querySelectorAll(".live-example.output"),
    (output) => element.compareDocumentPosition(output) & Node.DOCUMENT_POSITION_FOLLOWING
  ).querySelector("div");
  return {
    js,
    html,
    output,
  };
}

async function executeExample({ js, html, output }) {
  const fn = new AsyncFunction(
    "root", "target", "mount", "unmount", "move", "createSource", "observable", "computed",
    "registerTemplate", "parseMustache", js);
  const state = await fn(
    root, target, mount, unmount, move, observable, observable, computed,
    registerTemplate, parseMustache);
  const template = parseMustache(html);
  const [_, fragment] = s2(state, template);
  output.innerHTML = "";
  output.appendChild(fragment);
}

// Process line numbers
for (const pre of document.querySelectorAll("pre")) {
  pre.innerHTML = pre.textContent
    .split("\n").map(line => `<code>${line}</code>`).join("");
}
