package main

import (
	"fmt"
	"os"
	"sort"
)

var outType []int  // 切片
var keyType []bool // 切片

type ExcelData struct {
	name string
	Rows [][]string
}

func main() {

	args := os.Args
	fmt.Println(args, len(args))
	if len(args) < 3 {
		fmt.Println("传入的参数不足,确保有输入文件和输出地址")
		return
	}
	file := args[1]
	outPath := args[2]
	data, err := Read(file)
	if err != nil {
		fmt.Println("读取数据错误 ", err)
		return
	}
	// 写入erl
	WriteErl(data, outPath)

	// todo 前端配置可根据实际情况导出

	var input string
	fmt.Scanln("输入任意按键退出", &input)
	return
}

func In(target string, Array []string) bool {
	sort.Strings(Array)
	index := sort.SearchStrings(Array, target)
	if index < len(Array) && Array[index] == target {
		return true
	}
	return false
}
