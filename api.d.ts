type PrimitiveKey = string | number | symbol;

interface DataObject {
    [key: PrimitiveKey]: any,
}

// Mainly for annotation purposes.
interface Proxy {
    [key: PrimitiveKey]: any,
}

type S2Result = [
  proxy: Proxy,
  fragment: DocumentFragment,
];

export function registerTemplate(
  name: string,
  template: string | Element
): Element;

export function parseMustache(html: string): Element;

export function html(
  template: TemplateStringsArray,
  ...props: Array<string | Element>
): Element;

export function observable(obj?: DataObject, shouldPartiallyReplace?: boolean): Proxy;
export function computed(definition: DataObject): DataObject;
export function ref(obj: DataObject): DataObject;

export default function s2(obj: DataObject, template: Element): S2Result;

export const root: symbol;
export const target: symbol;
export const mount: symbol;
export const unmount: symbol;
export const move: symbol;
