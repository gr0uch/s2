import bind, { registerTemplate, mount, unmount } from "./main.mjs";
import { observable, createComputed, ref } from "./computed-properties.mjs";
import { createMustacheTag } from "./mustache.mjs";
import hljs from 'https://esm.run/highlight.js/lib/core';
import javascript from 'https://esm.run/highlight.js/lib/languages/javascript';

hljs.registerLanguage("javascript", javascript);

for (const el of document.querySelectorAll("pre code")) {
  hljs.highlightElement(el);
}

for (const el of document.querySelectorAll("[id]")) {
  const a = document.createElement("a");
  a.href = `#${el.id}`;
  a.textContent = "#";
  el.appendChild(a);
}

const html = createMustacheTag(registerTemplate);
const computed = createComputed(mount, unmount);

// uhhh..
