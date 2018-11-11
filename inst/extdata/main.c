#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <uv.h>

void print_stat(char *path, uv_loop_t *loop)
{
  uv_fs_t req;
  
  uv_fs_lstat(loop, &req, path, NULL);

  uv_stat_t st = req.statbuf;
  
  printf(",%llu", st.st_dev);
  printf(",%lld", st.st_mode);
  printf(",%lld", st.st_nlink);
  printf(",%lld", st.st_rdev);
  printf(",%lld", st.st_ino);
  printf(",%lld", st.st_size);
  printf(",%lld", st.st_blksize);
  printf(",%lld", st.st_blocks);
  printf(",%lld", st.st_flags);
  printf(",%lld", st.st_gen);
  printf(",%ld", st.st_atim.tv_sec);
  printf(",%ld", st.st_mtim.tv_sec);
  printf(",%ld", st.st_ctim.tv_sec);
  printf(",%ld", st.st_birthtim.tv_sec);
  
  uv_fs_req_cleanup(&req);
}

void dir_recursively(char *path, uv_loop_t *loop, int depth)
{
  char buffer[1024];
  uv_fs_t req;
  uv_dirent_t dent;
  
  int result = uv_fs_scandir(loop, &req, path, 0, NULL);
  
  if (depth == 0 && result > 0) {
   
   printf("directory,file,type,dev,mode,nlink,rdev,ino,size,blksize,blocks");
   printf(",flags,gen,atim,mtim,ctim,birthtim\n");
  }
  
  while (UV_EOF != uv_fs_scandir_next(&req, &dent)) {
    
    printf("\"%s\",\"%s\"", path, dent.name);
    printf(",%d", dent.type);

    strcpy(buffer, path);
    strcat(buffer, "/");
    strcat(buffer, dent.name);
    
    print_stat(buffer, loop);
    
    printf("\n"); 
    
    if (dent.type == UV_DIRENT_DIR) {
      
      dir_recursively(buffer, loop, depth + 1);
    } 
    
  } /* end of while */
  
  uv_fs_req_cleanup(&req);
}

int main(int argc, char **argv)
{
  if (argc < 2) {

    printf("Usage: ./main <path>\n");
    return -1;
  }

  uv_loop_t *loop = malloc(sizeof(uv_loop_t));
  uv_loop_init(loop);
  
  dir_recursively(argv[1], loop, 0);
  
  uv_run(loop, UV_RUN_DEFAULT);
  
  uv_loop_close(loop);
  free(loop);
  return 0;
}
