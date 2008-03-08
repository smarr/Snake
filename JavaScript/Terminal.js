

function Term(outputNode, w, h)  {
	this.KEY_UP    = 38;
	this.KEY_DOWN  = 40;
	this.KEY_LEFT  = 37;
	this.KEY_RIGHT = 39;
	var x = 0;
	var y = 0;
	var out = outputNode;
	var width = w;
	var height = h;
	
	this.cursorTo = function(newX, newY) {
		x = newX;
		y = newY;
	}
	
	this.put = function(str) {
		str = String(str);
		var pos = y * width + x;
		setAt(pos, str);
		pos = pos + str.length;
		x = pos % width;
		y = (pos - x)/width;
	}
	
	function setAt(pos, str) {
		start = "";
		end = "";
		if (pos > 0) {
			start = out.nodeValue.substring(0, pos);
		}
		end = out.nodeValue.substring(pos + str.length, out.nodeValue.length);
		out.nodeValue = start + str + end;
	}
}