// The wall-clock time, in ISO 8601 (UTC): a run's `created` field.
export const nowIsoImpl = () => new Date().toISOString();
