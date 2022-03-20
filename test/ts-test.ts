import s2, {
    html, computed, root, target, mount, unmount, move,
    registerTemplate, parseMustache, observable, ref,
} from "../api";

const s = observable(computed({
    foo: {
        bar: true,
    },
    wat() {
        return `hi ${this.foo.bar}`;
    },
}), true);

const t = html`<div>{{wat}}</div>`;

const [proxy, fragment] = s2(s, t);

document.body.appendChild(fragment);
