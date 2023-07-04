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
	typeRow := rows[1]
	keyRow := rows[3]
	// 每个字段的导出类型 1:前端导出 2:后端导出 3:都导出
	// 这里也可以使用颜色填充，可以少一行导出的配置
	typeArr := []string{"1", "2", "3"}
	for i, t := range typeRow {
		// 导出类型
		var v = 0
		if In(t, typeArr) {
			v, err = strconv.Atoi(t)
			if err != nil {
				fmt.Println("导出类型配置错误 ", i+1, t)
			}
		}

		//cellName, err := excelize.CoordinatesToCellName(i+1, 2)
		//if err != nil {
		//	fmt.Println("CoordinatesToCellName error", err)
		//}
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

	// 保存一下文件名
	fileName := filepath.Base(file)
	fileNameWithoutExt := fileName[:len(fileName)-len(filepath.Ext(fileName))]
	return ExcelData{
		name: fileNameWithoutExt,
		Rows: rows,
	}, nil
}

func getCellBgColor(f *excelize.File, sheet, cell string) string {
	styleID, err := f.GetCellStyle(sheet, cell)
	if err != nil {
		return err.Error()
	}
	fillID := *f.Styles.CellXfs.Xf[styleID].FillID
	fgColor := f.Styles.Fills.Fill[fillID].PatternFill.FgColor
	if fgColor != nil && f.Theme != nil {
		if clrScheme := f.Theme.ThemeElements.ClrScheme; fgColor.Theme != nil {
			if val, ok := map[int]*string{
				0: &clrScheme.Lt1.SysClr.LastClr,
				1: &clrScheme.Dk1.SysClr.LastClr,
				2: clrScheme.Lt2.SrgbClr.Val,
				3: clrScheme.Dk2.SrgbClr.Val,
				4: clrScheme.Accent1.SrgbClr.Val,
				5: clrScheme.Accent2.SrgbClr.Val,
				6: clrScheme.Accent3.SrgbClr.Val,
				7: clrScheme.Accent4.SrgbClr.Val,
				8: clrScheme.Accent5.SrgbClr.Val,
				9: clrScheme.Accent6.SrgbClr.Val,
			}[*fgColor.Theme]; ok && val != nil {
				return strings.TrimPrefix(excelize.ThemeColor(*val, fgColor.Tint), "FF")
			}
		}
		return strings.TrimPrefix(fgColor.RGB, "FF")
	}
	return "FFFFFF"
}
