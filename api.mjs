import s2, { root, target, mount, unmount, move, registerTemplate } from "./dist/main.mjs";
import parseMustache, { createMustacheTag } from "./dist/mustache.mjs";
import { observable, createComputed, ref } from "./dist/computed-properties.mjs";

export const html = createMustacheTag(registerTemplate);
export const computed = createComputed(mount, unmount);

export default s2;
export {
  root, target, mount, unmount, move, registerTemplate,
  parseMustache, observable, ref,
};
