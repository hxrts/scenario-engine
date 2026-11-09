export const processIdentity = () => ({ pid: String(process.pid), started: String(Math.round(performance.timeOrigin)) });

// EPERM means the process exists under another user.
export const processAlive = (pid) => () => {
  try {
    process.kill(pid, 0);
    return true;
  } catch (err) {
    return err.code === "EPERM";
  }
};
