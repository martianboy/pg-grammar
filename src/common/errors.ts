export const ERROR = 20;

export const ERRCODE_SYNTAX_ERROR = 0x0100;
export const ERRCODE_FEATURE_NOT_SUPPORTED = 0x0A00;

export function ereport(level: number, ...rest: unknown[]) {
    throw new Error();
}
export function errcode(sqlerrcode: number) { return 0; }
export function errmsg(msg: string): number { return 0; }
export function parser_errposition(pos: number) { return 0; }
