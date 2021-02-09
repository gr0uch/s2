import s2 from "../dist/main.js";

const obj = {};

const [node, proxy] = s2(obj, document.getElementById("menu"));

document.body.appendChild(element);

window.obj = obj;
window.p = proxy;
