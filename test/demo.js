import s2 from "../dist/main.js";

const template = document.getElementById("menu");
const [node, proxy] = s2({
  title: "Hello, <em>world!</em>",
  easterEgg() {
    console.log("henlo", this);
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
