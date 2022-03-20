import s2, { html, computed, observable, } from "../api.mjs";
const s = observable(computed({
    foo: {
        bar: true,
    },
    wat() {
        return `hi ${this.foo.bar}`;
    },
}), true);
const t = html `<div>{{wat}}</div>`;
const [proxy, fragment] = s2(s, t);
document.body.appendChild(fragment);
