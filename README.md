# excelExporter
export excel to erlang file

示例vip.xlsx导出为 cfg_vip.erl。
导出的同时会根据导出的类型生成一个record 写入def.hrl中
  写入的时候会去判断是否有这个record，如果包含则会覆盖，没有则追加在文件末尾

