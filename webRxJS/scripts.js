(function (global, $, Rx) {
	
	var requestStream = Rx.Observable.from(testList);
	var source;

	function main() {
		var clicks$ = Rx.Observable.fromEvent($("#resetBttn"), 'click').subscribe (function () {
			location.reload();
		});
		var obs1 = Rx.Observable.fromEvent($("#frameworkDropdown"), 'change').map(function (e) {return e.target.value;}).startWith($('#frameworkDropdown').val());;
		var obs2 = Rx.Observable.fromEvent($("input[name=tstName]"), "keyup").map(function (e) {return e.target.value;}).startWith($('#nameField').val());;
		var obs3 = Rx.Observable.fromEvent($("#reusable"), "change").map(function (e) {return $("#reusable").is(':checked')}).startWith($('#reusable').is(':checked'));;
		var obs4 = Rx.Observable.fromEvent($("html"), "keyup").filter(e => e.keyCode == 8).startWith(false);
		var source = obs1.combineLatest(obs2, function(o1, o2){
			var ar = [o1, o2, ""];
			return ar;
		}).combineLatest(obs3, function(m, o3){
			m[2] = o3;
			return m;
		}).combineLatest(obs4, function(m, o4){
			return m;
		}).flatMapLatest(filter);
		
		var subscription = source.subscribe(
		function rndr (x) {		
		    var $tstDiv = $("<div></div>").attr("id", "tstid"+x.nr).addClass("tstContainer");
			var $tstName = $("<button></button>").addClass("tstbtn").css("width","100%").text("#" + x.nr + ": " + x.name);
			var $tstDesc = $("<div></div>").addClass("paramContainer").text(x.description);
			$tstDiv.append($tstName).append($tstDesc);
			if(x.reusable){
				$tstDiv.css("border-color", "#00f")
			}
			if(x.qc){
				$tstDiv.css("border-color", "#4F0")
			}
			$(".dispField").append($tstDiv);
			if(x.nr == testList.length){
			obsdetail ();
			}
		},
		function (err) {
			console.log('Error: ' + err);
		},
		function () {
			console.log('Completed');
		});
		
		function filter(params){
			$(".dispField").empty();
			return requestStream
				.filter(function(result) {
					if(params[0] == "All"){
						return result;
					} else {
						return result.type === params[0].toUpperCase();
					}
				})
				.filter(result => result.name.toString().toUpperCase().indexOf(params[1].toUpperCase()) > -1)
				.filter(function(result) {
					if (params[2] == true){
						return result.reusable === params[2];
					}else{
						return result;
					}
				})
		}
		
// ----------------------------------------------------------------------------------------------------------------------------------
		function obsdetail () {
			var obsDet = Rx.Observable.fromEvent($(".tstbtn"), 'click').map(function (e) {
				return $( e.target ).parent().attr("id");
				}).flatMapLatest(detail);
			
			var subscription2 = obsDet.subscribe(
			function (x) {			
				$(".dispField").empty();
				
				var $tstName = $("<div class=\"paramContainer\"></div>").html("<div class=\"nmlbl\">Name: </div><div>#" + x.nr + ": " + x.name + "</div>");
				var $tstPath = $("<div class=\"paramContainer\"></div>").html("<div class=\"nmlbl\">Path: </div><div>"  + x.path + "</div>");
				var $tstContact = $("<div class=\"paramContainer\"></div>").html("<div class=\"nmlbl\">Contact: </div><div>" + x.contact + "</div>");
				var $tstLastUpdate = $("<div class=\"paramContainer\"></div>").html("<div class=\"nmlbl\">Last Update: </div><div>" + x.LastUpdate + "</div>");
				var $tstExtData = $("<div class=\"paramContainer\"></div>").html("<div class=\"nmlbl\">External DataSheet: </div><div>" + x.extData + "</div>");
				var $tstDesc = $("<div class=\"paramContainer\"></div>").html("<div class=\"nmlbl\" style=\"height: 50%;\">Description: </div><div>" + x.description + "</div>");
				var $backBttn = $("<button></button>").attr("id", "backBttn").attr("type","button").text("Back");
				$(".dispField").append($tstName).append($tstPath).append($tstContact).append($tstLastUpdate).append($tstExtData).append($tstDesc).append($backBttn);
				
				var clicks$ = Rx.Observable.fromEvent($("#backBttn"), 'click').subscribe (function () {
						$("html").trigger({type: 'keyup', which: 8, keyCode: 8});
					});
				},
			function (err) {
				console.log('Error: ' + err);
			},
			function () {
				console.log('Completed');
			});
			
			function detail (detId) {
				var id = detId.substr(5);
				return requestStream
					.filter(result => result.nr == id);
			}
		}
	}
	$(main);
}(window, jQuery, Rx));