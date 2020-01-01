export const TIMESTAMP_MASK = (b: number) => (1 << (b))
export const INTERVAL_MASK = (b: number) => (1 << (b))

/* Macros to handle packing and unpacking the typmod field for intervals */
export const INTERVAL_FULL_RANGE = 0x7FFF
export const INTERVAL_RANGE_MASK = 0x7FFF
export const INTERVAL_FULL_PRECISION = 0xFFFF
export const INTERVAL_PRECISION_MASK = 0xFFFF
export const INTERVAL_TYPMOD = (p: number, r: number) => ((((r) & INTERVAL_RANGE_MASK) << 16) | ((p) & INTERVAL_PRECISION_MASK))
export const INTERVAL_PRECISION = (t: number) => ((t) & INTERVAL_PRECISION_MASK)
export const INTERVAL_RANGE = (t: number) => (((t) >> 16) & INTERVAL_RANGE_MASK)
