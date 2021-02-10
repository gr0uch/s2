import s2 from "../dist/main.js";

const template = document.getElementById("menu");
const [node, binding] = s2({
  title: "Hello, <em>world!</em>",
  easterEgg() {
    console.log("henlo", this.title);
  },
  counter: {
    count: 0,
    increment() {
      this.count++;
    },
  },
}, template);
document.body.appendChild(node);

window.p = binding;
