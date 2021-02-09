import s2 from "../dist/main.js";

const obj = {};

const [element, proxy] = s2(obj, document.getElementById("menu"));

window.obj = obj;
window.p = proxy;
