"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
/* eslint-disable @typescript-eslint/no-require-imports */
const fs = __importStar(require("fs"));
const path = __importStar(require("path"));
const testsDir = __dirname;
const files = fs.readdirSync(testsDir).filter(f => f.endsWith('.ts') && !f.endsWith('.d.ts') && f !== 'run-tests.ts');
let failed = 0;
for (const f of files) {
    const p = path.join(testsDir, f);
    try {
        console.log('running', f);
        require(p);
        console.log('ok', f);
    }
    catch (e) {
        failed++;
        const msg = e instanceof Error ? (e.stack || e.message) : String(e);
        console.error('FAIL', f, msg);
    }
}
if (failed) {
    console.error(`${failed} test(s) failed`);
    process.exit(1);
}
else {
    console.log('All tests passed');
}
//# sourceMappingURL=run-tests.js.map