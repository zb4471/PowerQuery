let
    源 = 发票路径,
    第一个Sheet = Table.SelectRows(源, each [Kind] = "Sheet"){0}[Data],

    删除右侧空白列 = 
        let
            列名 = Table.ColumnNames(第一个Sheet),
            列值列表 = Table.ToColumns(第一个Sheet), // 所有列的值，是一个list嵌套list，不包含列名
            布尔列表 = List.Transform(列值列表, each List.Contains(_, "value for customs", Comparer.OrdinalIgnoreCase)),
            尾列序号 = List.PositionOf(布尔列表, true) + 1,
            结果表 = if 尾列序号 = 0 then error "未找到包含 'value for customs' 的列，请检查数据！"
                else Table.SelectColumns(第一个Sheet, List.FirstN(列名, 尾列序号))
        in
            结果表,
    标题1列名 = {"Product ID", "Product NAME", "No. of Units", "Quantity", "Commodity Code", "CoO", "Intended Use", "Value for Customs"},
    标题1小写 = List.Transform(标题1列名, Text.Lower),
    标题1键值对 = Record.FromList(标题1列名, 标题1小写),
    标题2列名 = {"Product Id", "AbID", "Labelled as", "Selling Size"},     
    标题2小写 = List.Transform(标题2列名, Text.Lower),
    标题2键值对 = Record.FromList(标题2列名, 标题2小写),
    添加标记列 =
        Table.AddColumn(删除右侧空白列, "标记", each
            let
                行字段值列表 = Record.FieldValues(_), // 是这个Record的所有Value的list，和Record.ToList完全相同
                转化为文本  = List.Transform(行字段值列表, Text.From),
                行字符串 = Text.Trim(Text.Combine(转化为文本, ",")), // Combine会在内部将 null 转换为 ""，不放心的话可以显式增加 ?? ""
                第一个值 = Text.From(行字段值列表{0}) ?? "", // 将 null 转换为 ""，以便进行Text.操作
                标记 = 
                    if 行字符串 = "" then "空行"
                    else if Text.StartsWith(第一个值, "AB", Comparer.OrdinalIgnoreCase) and Text.Contains(第一个值, "-100") then "AB行"
                    else if List.AllTrue(List.Transform(标题1列名, each Text.Contains(行字符串, _, Comparer.OrdinalIgnoreCase))) then "标题1" // 将关键字列表转化为一个true false列表
                    else if List.AllTrue(List.Transform(标题2列名, each Text.Contains(行字符串, _, Comparer.OrdinalIgnoreCase))) then "标题2"
                    else if Text.Contains(行字符串, "Date:") then "日期"
                    else if Text.Contains(行字符串, "Order Reference Number") then "参考号"
                    else if Text.Contains(行字符串, "Total Goods") then "总货值"
                    else if Text.Contains(行字符串, "Invoice Total") then "总金额"
                    else "无关数据"
            in
                标记),

    添加索引列 = Table.AddIndexColumn(添加标记列, "索引", 1),

    获取表体1 = 
        let
            标题1行 = Table.SelectRows(添加索引列, each [标记] = "标题1"),
            标题索引 = if Table.RowCount(标题1行) = 0 then error "未找到 标题1行，请检查数据！" else 标题1行{0}[索引],

            标题2行 = Table.SelectRows(添加索引列, each [标记] = "标题2"),
            结尾索引 = if Table.RowCount(标题2行) = 0 then error "未找到 标题2行，请检查数据！" else 标题2行{0}[索引]
        in
            Table.SelectRows(添加索引列, each [索引] >= 标题索引 and [索引] < 结尾索引),

    获取表体2 = 
        let
            标题索引 = Table.SelectRows(添加索引列, each [标记] = "标题2"){0}[索引],
            结尾行 = Table.SelectRows(添加索引列, each [标记] = "空行" and [索引] >= 标题索引), // 标题2下面的第一个空行
            结尾索引 = 结尾行{0}[索引]
        in
            Table.SelectRows(添加索引列, each [索引] >= 标题索引 and [索引] < 结尾索引),
    
    获取总金额表 = 
        let
            总货值行 = Table.SelectRows(添加索引列, each [标记] = "总货值"),
            总货值行索引 = if Table.RowCount(总货值行) = 0 then error "未找到总货值行 Total Goods，请检查数据！" else 总货值行{0}[索引],
            总金额行 = Table.SelectRows(添加索引列, each [标记] = "总金额"),
            总金额行索引 = if Table.RowCount(总金额行) = 0 then error "未找到总金额行 Invoice Total，请检查数据！" else 总金额行{0}[索引],
            
            表体 = Table.SelectRows(添加索引列, each [索引] >= 总货值行索引 and [索引] <= 总金额行索引),
            列名 = Table.ColumnNames(表体),
            空白列名 = List.Select(列名, each List.IsEmpty(List.RemoveItems(Table.Column(表体, _), {"", null}))),
            删除空白列 = Table.RemoveColumns(表体, 空白列名 & {"标记", "索引"})
        in
            删除空白列,

    fun_表格格式化 = (输入表 as table, 列名键值对 as record) =>
        let
            保留数据行 = Table.SelectRows(输入表, each Text.StartsWith([标记], "标题") or ([标记] = "AB行")), // 仅保留标题行和AB行
            删除标记和索引 = Table.RemoveColumns(保留数据行, {"标记", "索引"}),
            提升标题 = Table.PromoteHeaders(删除标记和索引),
            实际列名 = Table.ColumnNames(提升标题),
            指定列名 = Record.ToList(列名键值对), // 这是规定的大小写形式
            缺失列 = List.Difference(指定列名, 实际列名, Comparer.OrdinalIgnoreCase), // 找出不存在的列名
            保留标准列 =
                if List.Count(缺失列) > 0 then error "表格中缺少以下列名：" & Text.Combine(缺失列, ", ")
                else
                    let
                        标准化列名 = Table.TransformColumnNames(提升标题, each Record.FieldOrDefault(列名键值对, Text.Lower(_), _)) // 将实际列名转化为小写，然后通过键值对映射为规定大小写，并改写ColumnNames。未指定的列，比如 Column2 之类的列，名称不变 // 很妙，细细品味
                    in
                        Table.SelectColumns(标准化列名, 指定列名) // 仅保留指定列
        in
            保留标准列,
    
    表格1格式化 = fun_表格格式化(获取表体1, 标题1键值对),
    表格2格式化 = fun_表格格式化(获取表体2, 标题2键值对),

    表格1行数 = Table.RowCount(表格1格式化),
    表格2行数 = Table.RowCount(表格2格式化),    

    合并两个表 =
        if Table.RowCount(表格1格式化) <> Table.RowCount(表格2格式化) then error "两个表格的行数不同，请检查！"
        else
            Table.FromColumns(
                Table.ToColumns(表格1格式化) & Table.ToColumns(表格2格式化), Table.ColumnNames(表格1格式化) & Table.ColumnNames(表格2格式化)
            ),

    提取币种 =
        let
            添加币种列 = Table.AddColumn(合并两个表, "币种", each Text.Start([Value for Customs], 3)),
            币种列表 = List.Distinct(添加币种列[币种]), // 币种列去重
            币种数量 = List.Count(币种列表)
        in
            if 币种数量 > 1 then error "币种不唯一，请检查！"
            else 添加币种列,

    添加发票序号 = Table.AddIndexColumn(提取币种, "发票序号", 1),

    校验完整AB货号 =
        let
            添加校验列 = Table.AddColumn(添加发票序号, "校验", each [Product ID] = [Product Id]),
            不一致的行 = Table.SelectRows(添加校验列, each [校验] = false),
            不一致的行号 = Table.Column(不一致的行, "发票序号"),
            合并错误行号 = Text.Combine(List.Transform(不一致的行号, Text.From), ", ")
        in
            if List.Count(不一致的行号) > 0 then error "以下行号的完整AB货号不一致：" & 合并错误行号 & "。"
            else 添加发票序号,

    校验短AB号 = 
        let
            添加校验列 = Table.AddColumn(校验完整AB货号, "校验", each Comparer.Equals(Comparer.OrdinalIgnoreCase, [AbID], Text.BeforeDelimiter([Product ID], "-100"))),
            不一致的行 = Table.SelectRows(添加校验列, each [校验] = false),
            不一致的行号 = Table.Column(不一致的行, "发票序号"),
            合并错误行号 = Text.Combine(List.Transform(不一致的行号, Text.From), ", ")
        in
            if List.Count(不一致的行号) > 0 then error "以下行号的短AB货号不一致：" & 合并错误行号 & "。"
            else 校验完整AB货号,

    校验Pcs列 = 
        let
            添加校验列 = Table.AddColumn(校验短AB号, "校验", each Text.Contains([No. of Units], "Pc", Comparer.OrdinalIgnoreCase) or Text.Contains([No. of Units], "Pcs", Comparer.OrdinalIgnoreCase)),
            不一致的行 = Table.SelectRows(添加校验列, each [校验] = false),
            不一致的行号 = Table.Column(不一致的行, "发票序号"),
            合并错误行号 = Text.Combine(List.Transform(不一致的行号, Text.From), ", ")
        in
            if List.Count(不一致的行号) > 0 then error "以下行号的数量格式Pcs不正确：" & 合并错误行号 & "。"
            else 校验短AB号,

    校验EA列 = 
        let
            添加校验列 = Table.AddColumn(校验Pcs列, "校验", each Text.Contains([Quantity], "EA", Comparer.OrdinalIgnoreCase)),
            不一致的行 = Table.SelectRows(添加校验列, each [校验] = false),
            不一致的行号 = Table.Column(不一致的行, "发票序号"),
            合并错误行号 = Text.Combine(List.Transform(不一致的行号, Text.From), ", ")
        in
            if List.Count(不一致的行号) > 0 then error "以下行号的数量格式EA不正确：" & 合并错误行号 & "。"
            else 校验Pcs列,

    校验数量并添加Qty列 =
        let
            添加Qty = Table.AddColumn(校验EA列, "Qty", each Int16.From(Text.BeforeDelimiter(Text.Upper([Quantity]), "EA")), Int16.Type),                
            添加数量Pcs = Table.AddColumn(添加Qty, "数量Pcs", each Int16.From(Text.BeforeDelimiter(Text.Upper([No. of Units]), "PC"))),
            添加校验列 = Table.AddColumn(添加数量Pcs, "校验", each [Qty] = [数量Pcs]),
            不一致的行 = Table.SelectRows(添加校验列, each [校验] = false),
            不一致的行号 = Table.Column(不一致的行, "发票序号"),
            合并错误行号 = Text.Combine(List.Transform(不一致的行号, Text.From), ", ")
        in
            if List.Count(不一致的行号) > 0 then error "以下行号的两个数量不一致：" & 合并错误行号 & "。"
            else 添加Qty,

    添加Amount列 = Table.AddColumn(校验数量并添加Qty列, "Amount", each Number.FromText(Text.Middle([Value for Customs], 3)), type number),
 
    获取总数量 = List.Sum(Table.Column(添加Amount列, "Qty")),

    获取总金额 = // 是一个数字
        let
            金额加总 = Number.Round(List.Sum(Table.Column(添加Amount列, "Amount")), 4), // 四舍五入，避免很长的小数点

            金额表列名 = Table.ColumnNames(获取总金额表),
            金额表第一行 = Table.ToRows(获取总金额表){0},
            第一行小写 = List.Transform(金额表第一行, Text.Lower),
            TotalGoods所在列索引 = List.PositionOf(第一行小写, "total goods"),
            金额所在列索引 = TotalGoods所在列索引 + 1,
            金额所在列名 = 金额表列名{金额所在列索引},

            总货值文本 = Record.Field(获取总金额表{0}, 金额所在列名),
            总货值 = Number.FromText(Text.Middle(总货值文本, 3)),

            总金额文本 = Record.Field(获取总金额表{2}, 金额所在列名),
            总金额 = Number.FromText(Text.Middle(总货值文本, 3))

        in
            if 总货值 <> 金额加总 then error "总货值 Total Goods 不正确，请检查！"
            else if 总金额 <> 金额加总 then error "总金额 Invoice Total 不正确，请检查！"
            else 金额加总,
    
    // 获取日期 = // 是一个日期
    //     let
    //         日期行 = Table.SelectRows(添加索引列, each [标记] = "日期"){0},
    //         日期行值列表 = Record.FieldValues(日期行),
    //         日期文本 = List.Select(日期行值列表, each Text.StartsWith(Text.From(_), "Date:", Comparer.OrdinalIgnoreCase)){0},
    //         日期 = Date.FromText(Text.AfterDelimiter(Text.Lower(日期文本), "date:"))
    //     in
    //         日期,

    获取RefNumber = // 是一个文本
        let
            参考号所在行 = Table.SelectRows(添加索引列, each [标记] = "参考号"){0},
            参考号行列表 = Record.FieldValues(参考号所在行),
            参考号文本 = List.Select(参考号行列表, each Text.StartsWith(Text.From(_), "Order Reference Number", Comparer.OrdinalIgnoreCase)){0},
            参考号 = Text.Clean(Text.Trim(Text.AfterDelimiter(Text.Lower(参考号文本), "order reference number:"))),
            参考号清理 = Text.Clean(Text.Trim(参考号))
        in
            参考号清理,
    
    输出结果 = [
        
        发票格式化 = 添加Amount列,
        参考号 = 获取RefNumber,
        总金额 = 获取总金额,
        总数量 = 获取总数量
        // 日期 = 获取日期

    ]
in
    输出结果
