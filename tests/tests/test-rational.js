"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
/* eslint-disable @typescript-eslint/no-unused-vars */
const test_1 = require("./test");
const rational_1 = __importDefault(require("../dist/rational"));
const string_1 = require("../dist/string");
(0, test_1.test)('rational arithmetic', () => {
    const a = (0, rational_1.default)(3, 4);
    const b = (0, rational_1.default)(2, 3);
    (0, test_1.expect)(a.add(b)).toEqual((0, rational_1.default)(17, 12));
    (0, test_1.expect)(a.sub(b)).toEqual((0, rational_1.default)(1, 12));
    (0, test_1.expect)(a.mul(b)).toEqual((0, rational_1.default)(1, 2));
    (0, test_1.expect)(a.div(b)).toEqual((0, rational_1.default)(9, 8));
    (0, test_1.expect)(a.mod(b)).toEqual((0, rational_1.default)(1, 12)); //1/8 * 2/3
    const eqn = (0, string_1.parse)(rational_1.default, '3 / 4');
    (0, test_1.expect)(eqn).toEqual((0, rational_1.default)(3, 4));
});
//# sourceMappingURL=test-rational.js.map