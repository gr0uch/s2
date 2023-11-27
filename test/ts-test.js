"use strict";
var __makeTemplateObject = (this && this.__makeTemplateObject) || function (cooked, raw) {
    if (Object.defineProperty) { Object.defineProperty(cooked, "raw", { value: raw }); } else { cooked.raw = raw; }
    return cooked;
};
Object.defineProperty(exports, "__esModule", { value: true });
var api_1 = require("../api");
var s = (0, api_1.observable)((0, api_1.computed)({
    foo: {
        bar: true,
    },
    wat: function () {
        return "hi ".concat(this.foo.bar);
    },
}), true);
api_1.default.debug = true;
var t = (0, api_1.html)(templateObject_1 || (templateObject_1 = __makeTemplateObject(["<div>{{wat}}</div>"], ["<div>{{wat}}</div>"])));
var _a = (0, api_1.default)(s, t), proxy = _a[0], fragment = _a[1];
document.body.appendChild(fragment);
var templateObject_1;
