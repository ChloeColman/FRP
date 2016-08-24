$lx = Get-ChildItem Q:\ -recurse -filter "Default.xls" | % { $_.FullName }
$outItems = New-Object System.Collections.Generic.List[System.Object]

$xl = New-Object -comobject Excel.Application
$xl.Visible = $true
$cnt = 1

foreach($lf in $lx){

    if ($lf.ToUpper().Contains("NC")){
        $tp = "NC"
    } elseif ($lf.ToUpper().Contains("3270")){
        $tp = "3270"
    } elseif ($lf.ToUpper().Contains("TOUCH")){
        $tp = "TOUCH"
    } else {
        $tp = "OTHER"
    }

    if ($lf.ToUpper().Contains("REUSABLE")){
        $re = $true
    } else {
        $re = $false
    }

    if ($lf.ToUpper().Contains("00_UFT_Tests")){
        $qc = $true
    } else {
        $qc = $false
    }
    
    $wb = $xl.Workbooks.Open($lf)
    
    try{    
        $ws = $wb.Sheets.Item('Description')
        $ct = $ws.Range("B2").Text
        $lu = $ws.Range("B3").Text
        $ed = $ws.Range("B4").Text
	    $de = $ws.Range("B5").Text
    } catch {
        $ct = " - "
        $lu = " - "
        $ed = " - "
	    $de = " - "
    }

    $path = $lf.Substring(0, $lf.LastIndexOf('\'))
    $name = $path.Split('\')[-1]

    $Output = [pscustomobject][ordered]@{
	    nr = $cnt
        name = $name
        path= $path
        contact = $ct
        LastUpdate = $lu
        extData = $ed
	    description = $de
        type = $tp
        reusable = $re
        qc = $qc
    }

    $cnt = $cnt + 1
	$outItems.add($Output)
	$wb.Close()
}
$outJson = $outItems | ConvertTo-Json -depth 999 | Out-File "web\data.js"
$x = Get-Content "web\data.js"
$x[0] = "var testList = ["
$x | Out-File "web\data.js"

$xl.Quit()
[System.Runtime.Interopservices.Marshal]::ReleaseComObject($xl)