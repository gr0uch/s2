import s2, { root, target, mount, unmount, move, registerTemplate } from "./main.min.mjs";
import { observable, createComputed } from "./computed-properties.min.mjs";
import parseMustache from "./mustache.min.mjs";

// s2.debug = true;

const computed = createComputed(mount, unmount);

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
}

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

function executeExample({ js, html, output }) {
    const fn = new Function(
      "root", "target", "mount", "unmount", "move", "createSource", "observable", "computed",
      "registerTemplate", "parseMustache", js);
    const state = fn(
      root, target, mount, unmount, move, observable, observable, computed,
      registerTemplate, parseMustache);
    const template = parseMustache(html);
    const [_, fragment] = s2(state, template);
    window.p = _;
    output.innerHTML = "";
    output.appendChild(fragment);
}

// Process line numbers
for (const pre of document.querySelectorAll("pre")) {
    pre.innerHTML = pre.textContent
        .split("\n").map(line => `<code>${line}</code>`).join("");
}
