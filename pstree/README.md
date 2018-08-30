This patch adds system call `int pstree(pid_t pid, int uid)` to Minix PM (Process Manager) server.

Function `pstree(pid, uid)` writes to stdout tree of processes, starting from process of pid owned by uid.

To apply patch, put pstree.patch file in `\` and use there command `patch -p1 < pstree.patch`.