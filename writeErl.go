package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strings"
)

func WriteErl(data ExcelData, path string) {
	isExport := writeHrl(data, path) // 写入hrl
	// 是否需要导出
	if !isExport {
		return
	}

	fileName := strings.ToLower(data.name)
	fileHead := "%%coding: latin-1 \n" +
		"-module(cfg_" + fileName + ").\n" +
		//"-include(\"def.hrl\").\n" +
		//"-compile(export_all).\n\n" +
		"-export([get_value/1, get_value/2, get/1, all/0]). \n\n" +
		"get_value(Key) -> \n\t?MODULE:get(Key).\n\n" +
		"get_value(Key,Default) -> \n" +
		"\tcase ?MODULE:get(Key) of \n" +
		"\t\t undefined -> Default;\n" +
		"\t\tValue -> Value\n" +
		"\tend.\n"

	fileStr := ""
	allStr := "all() -> \n\t[\n\t\t"
	allFirstLine := true
	// 遍历行和列，
	for r, row := range data.Rows {
		if r >= 4 {
			recordStr := ""
			keyStr := ""
			multiKey := false
			for c, cell := range row {
				if outType[c] == 2 || outType[c] == 3 {
					if keyType[c] {
						if keyStr != "" {
							keyStr += ","
							multiKey = true
						}
						keyStr += cell
					}
					if recordStr != "" {
						recordStr += ","
					}
					recordStr += cell
				}
			}
			recordStr = "{" + fileName + "_def," + recordStr + "}"
			if multiKey {
				keyStr = "{" + keyStr + "}"
			}
			fileStr += "\nget(" + keyStr + ") ->\n\t" + recordStr + ";"
			if !allFirstLine {
				allStr += ",\n\t\t"
			}
			allFirstLine = false
			allStr += recordStr
		}
	}
	allStr += "\n\t]."
	fileStr += "\nget(_) -> \n\tundefined."

	fileStr = fileHead + fileStr + "\n\n" + allStr
	// todo 写入erl文件
	erlFile := path + "/cfg_" + fileName + ".erl"
	err := os.WriteFile(erlFile, []byte(fileStr), 0644)
	if err != nil {
		fmt.Println("写入文件失败", erlFile, err)
		return
	}
	//fmt.Println("file_string :", fileStr)
	fmt.Println(erlFile + " done")
}

func writeHrl(data ExcelData, path string) bool {
	var hrlStr = ""
	row := data.Rows[2]
	for i, c := range outType {
		if c == 1 || c == 3 {
			filed := row[i]
			if hrlStr != "" {
				hrlStr += ", "
			}
			hrlStr += strings.ToLower(filed)
		}
	}
	if hrlStr == "" {
		fmt.Println("不需要导出为erl")
		return false
	}
	fileName := strings.ToLower(data.name)
	noticestr := "%% 自动生成，切勿修改\n"
	hrlStr = "-record(" + fileName + "_def, {" + hrlStr + "})."
	//写入头文件
	//fmt.Println(">>>>> hrlStr:", hrlStr)

	hrlFile := path + "/def.hrl"
	// 如果头文件不存在
	_, err := os.Stat(hrlFile)
	if os.IsNotExist(err) {
		err := os.WriteFile(hrlFile, []byte(noticestr+hrlStr), 0644)
		if err != nil {
			fmt.Println("直接写入文件失败", hrlFile, err)
			return false
		}
		fmt.Println(hrlFile + "写入成功")
		return false
	}

	// 头文件存在，匹配字符串
	file, err := os.OpenFile(hrlFile, os.O_RDWR, 0644)
	if err != nil {
		fmt.Println("无法打开文件:", err)
		return false
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	pattern := `-record\(` + fileName + `_def,[^)]+\)\.`
	regex := regexp.MustCompile(pattern)
	var lines []string
	match := false
	for scanner.Scan() {
		line := scanner.Text()
		if regex.MatchString(line) {
			line = regex.ReplaceAllString(line, hrlStr)
			match = true
		}
		lines = append(lines, line)
	}
	if !match {
		lines = append(lines, "\n"+noticestr+hrlStr)
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("搜索头文件错误:", err)
		return false
	}

	// 重新写入
	file.Seek(0, 0)
	writer := bufio.NewWriter(file)
	for _, line := range lines {
		_, err := writer.WriteString(line + "\n")
		if err != nil {
			fmt.Println("写入头文件错误:", err)
			return false
		}
	}
	writer.Flush()

	fmt.Println(hrlFile + " done")
	return true
}
