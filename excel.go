package main

import (
	"fmt"
	"github.com/xuri/excelize/v2"
	"os"
	"sort"
	"strings"
)

var outType []int  // 切片
var keyType []bool // 切片

type ExcelData struct {
	file      *excelize.File
	name      string
	sheetName string
	Rows      [][]string
}

// var langType = []string{"CN", "KR", "TW", "TH", "JP", "US", "EU", "WU"}
var langMap = map[string]int{
	"KR": 2,
	"TW": 3,
	"TH": 4,
	"JP": 5,
	"US": 6,
	"EU": 7,
	"WU": 8,
	"CN": 1,
}

var langArr []string
var langKeys = make(map[string]string)

var langDataMap = make(map[string]langData)

type langData struct {
	marco  string
	record string
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

func GetCellBgColor(f *excelize.File, sheet string, col int, row int) string {
	cellName, err := excelize.CoordinatesToCellName(col+1, row+1)
	if err != nil {
		fmt.Println("CoordinatesToCellName error", err)
	}

	styleID, err := f.GetCellStyle(sheet, cellName)
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
