/* eslint-disable @typescript-eslint/no-require-imports */
import * as fs from 'fs';
import * as path from 'path';

const testsDir = __dirname;
const files = fs.readdirSync(testsDir).filter(f => f.endsWith('.ts') && !f.endsWith('.d.ts') && f !== 'run-tests.ts');

let failed = 0;
for (const f of files) {
    const p = path.join(testsDir, f);
    try {
        console.log('running', f);
        require(p);
        console.log('ok', f);
    } catch (e: unknown) {
        failed++;
        const msg = e instanceof Error ? (e.stack || e.message) : String(e);
        console.error('FAIL', f, msg);
    }
}

if (failed) {
    console.error(`${failed} test(s) failed`);
    process.exit(1);
} else {
    console.log('All tests passed');
}
