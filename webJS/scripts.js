var rndrList = testList;

window.onload = function () {
	rndrTsts(rndrList);
	bindSearchListeners();
};

function rndrTsts (rndrList){
	$(".dispField").empty();
	for (var i = 0; i < rndrList.length; i++){
		var $tstDiv = $("<div></div>").attr("id", "tstid"+rndrList[i].nr).addClass("tstContainer");
		var $tstName = $("<button></button>").addClass("tstbtn").css("width","100%").text("#" + rndrList[i].nr + ": " + rndrList[i].name);
		var $tstDesc = $("<div></div>").addClass("paramContainer").text(rndrList[i].description);
		$tstDiv.append($tstName).append($tstDesc);
		if(rndrList[i].reusable){
			$tstDiv.css("border-color", "#00f")
		}
		if(rndrList[i].qc){
			$tstDiv.css("border-color", "#4F0")
		}
		$(".dispField").append($tstDiv);
	}
	$(".tstbtn").on("click", function () {
		dispTestDetails($(this).parent().get(0).id.replace("tstid",""));
	});
}

function bindSearchListeners(){
	$("input[name=tstName]").on("input", function() {
		filter();
    });
	$('#frameworkDropdown').on('change', function() {
		filter();
	});
	$("#reusable").click( function(){
		filter();
	});
	$("#resetBttn").on("click", function () {
		$('#frameworkDropdown').val("All");
		$("input[name=tstName]").val("");
		$("#reusable").prop( "checked", false );
		filter();
	});
}

function filter(){
	rndrList = testList;
	rndrList = byReusable(rndrList);
	rndrList = byFramework(rndrList);
	rndrList = byName(rndrList);
	rndrTsts(rndrList);
}

function byName(rndrList){
	var rndrListFltrd = [];
	for (var i = 0; i < rndrList.length; i++){
		if (rndrList[i].name.toUpperCase().indexOf($('#nameField').val().toUpperCase()) > -1 ) {
			rndrListFltrd.push(rndrList[i]);
		}
	}
	return rndrListFltrd;
}

function byFramework(rndrList){
	var rndrListFltrd = [];
	if ($('#frameworkDropdown').val() == "All") {
		return rndrList;
	} else {
		for (var i = 0; i < rndrList.length; i++){
			if (rndrList[i].type.toUpperCase() == $('#frameworkDropdown').val().toUpperCase()) {
				rndrListFltrd.push(rndrList[i]);
			}
		}
		return rndrListFltrd;
	}
}

function byReusable(rndrList){
	var rndrListFltrd = [];
	if ($('#reusable').is(':checked')) {
		for (var i = 0; i < rndrList.length; i++){
			if (rndrList[i].reusable) {
				rndrListFltrd.push(rndrList[i]);
			}
		}
		return rndrListFltrd;
	} else {
		return rndrList;
	}
}

function dispTestDetails (tstNr){
	$(".dispField").empty();
	var tstDetail = getrndrListElementByNr(tstNr)
	
	var $tstName = $("<div class=\"paramContainer\"></div>").html("<div class=\"nmlbl\">Name: </div><div>#" + tstDetail.nr + ": " + tstDetail.name + "</div>");
	var $tstPath = $("<div class=\"paramContainer\"></div>").html("<div class=\"nmlbl\">Path: </div><div>"  + tstDetail.path + "</div>");
	var $tstContact = $("<div class=\"paramContainer\"></div>").html("<div class=\"nmlbl\">Contact: </div><div>" + tstDetail.contact + "</div>");
	var $tstLastUpdate = $("<div class=\"paramContainer\"></div>").html("<div class=\"nmlbl\">Last Update: </div><div>" + tstDetail.LastUpdate + "</div>");
	var $tstExtData = $("<div class=\"paramContainer\"></div>").html("<div class=\"nmlbl\">External DataSheet: </div><div>" + tstDetail.extData + "</div>");
	var $tstDesc = $("<div class=\"paramContainer\"></div>").html("<div class=\"nmlbl\" style=\"height: 50%;\">Description: </div><div>" + tstDetail.description + "</div>");
	var $backBttn = $("<button></button>").attr("id", "backBttn").attr("type","button").text("Back");
	$(".dispField").append($tstName).append($tstPath).append($tstContact).append($tstLastUpdate).append($tstExtData).append($tstDesc).append($backBttn);
	
	$("#backBttn").on("click", function () {
		filter();
	});
	$('html').keyup(function(e){
		if(e.keyCode == 8){
			filter();
		}
	});
}


function getrndrListElementByNr(index){
	for (var i = 0; i < rndrList.length; i++){
		if (rndrList[i].nr.toString() == index){
			return rndrList[i];
		}
	}
}