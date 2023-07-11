package main

import (
	"errors"
	"fmt"
	"github.com/xuri/excelize/v2"
	"path/filepath"
	"strconv"
	"strings"
)

func Read(file string) (ExcelData, error) {
	fmt.Println("loading:", file)

	f, err := excelize.OpenFile(file)
	if err != nil {
		fmt.Println("无法打开 Excel 文件：", err)
		return ExcelData{}, err
	}

	// 读取第一个工作表的所有单元格数据
	sheetName := f.GetSheetName(0)
	rows, err := f.GetRows(sheetName)
	if err != nil {
		fmt.Println("读取文件错误", sheetName, err)
		return ExcelData{}, err
	}
	if len(rows) == 0 {
		return ExcelData{}, errors.New("没有读取到内容")
	}
	err = setExportType(rows)
	if err != nil {
		return ExcelData{}, err
	}

	// 保存一下文件名
	fileName := filepath.Base(file)
	fileNameWithoutExt := fileName[:len(fileName)-len(filepath.Ext(fileName))]

	err = setLangKeys(f, sheetName, rows, fileNameWithoutExt)
	if err != nil {
		return ExcelData{}, err
	}

	return ExcelData{
		file:      f,
		name:      fileNameWithoutExt,
		sheetName: sheetName,
		Rows:      rows,
	}, nil
}

// 设置每一字段的导出标签
func setExportType(rows [][]string) error {
	typeRow := rows[1]
	keyRow := rows[3]
	// 每个字段的导出类型 1:前端导出 2:后端导出 3:都导出
	// 这里也可以使用颜色填充，可以少一行导出的配置
	typeArr := []string{"1", "2", "3"}
	err := errors.New("")
	for i, t := range typeRow {
		// 导出类型
		var v = 0
		if In(t, typeArr) {
			v, err = strconv.Atoi(t)
			if err != nil {
				fmt.Println("导出类型配置错误 ", i+1, t)
				return err
			}
		}

		//color := getCellBgColor(f, sheetName, cellName)
		//fmt.Println("====>>>>color ", cellName, color)

		outType = append(outType, v)
		// 是否为key
		keyType = append(keyType, keyRow[i] == "key")
	}

	// 如果没有指定key，则第一列默认为key
	hasKey := false
	for _, k := range keyType {
		if k {
			hasKey = true
		}
	}
	if !hasKey {
		for i, v := range outType {
			if v == 2 || v == 3 {
				keyType[i] = true
				break
			}
		}
	}
	return nil
}

// 记录多地区key
func setLangKeys(file *excelize.File, sheetName string, rows [][]string, fileName string) error {
	LangIndex := 0
	for i, row := range rows {
		if i >= 4 && len(row) > 0 {
			var recordStr, keyStr, keyStr2 string
			//keyStr := ""
			//keyStr2 := ""
			multiKey := false
			for k, cell := range row {
				// 判断地区标签
				v, ok := langMap[strings.ToUpper(cell)]
				if k == 0 && ok {
					color := GetCellBgColor(file, sheetName, k, i)
					if color == "FF0000" {
						LangIndex = v
						langArr = append(langArr, strings.ToUpper(cell))
						continue
					}
				}

				if outType[k] == 2 || outType[k] == 3 {
					if keyType[k] {
						if keyStr != "" {
							keyStr += ","
							keyStr2 += "_"
							multiKey = true
						}
						keyStr += cell
						keyStr2 += cell
					}
					if recordStr != "" {
						recordStr += ","
					}
					recordStr += cell
				}
			}
			if multiKey {
				keyStr = "{" + keyStr + "}"
			}
			recordStr = strings.Replace("{"+fileName+"_def,"+recordStr+"}", "\n", "", -1)
			if LangIndex != 0 {
				_, ok := langKeys[keyStr]
				if !ok && keyStr != "" {
					langKeys[keyStr] = keyStr2
				}
				//if !In(keyStr, langKeys) {
				//	langKeys = append(langKeys, keyStr) // 多地区控制的key
				//}

				// 记录每个地区对应的key-record
				langDataMap[keyStr+strconv.Itoa(LangIndex)] = langData{
					marco:  keyStr2,
					record: recordStr,
				}
			}
		}
	}
	return nil
}
