// Note that this code is documented using JSDoc and guided by the following URLs:
// http://code.google.com/p/jsdoc-toolkit/wiki/TagReference
// https://developers.google.com/closure/compiler/docs/js-for-compiler

// NOTE: The following code assumes that a global object, "gridSVGCoords"
//       is available. In other words, to use this code, load this object first.

/**
 * Returns a unit's x location relative to a viewport.
 *
 * @param {string} vpname The name of the viewport that the unit is drawn within
 * @param {number} x The size of the unit, based on 'from'
 * @param {string} from The input unit type
 * @param {string} to The output unit type (optional, defaults to "svg")
 * @returns {number} A unit in SVG pixels
 */
var viewportConvertX = function(vpname, x, from, to) {
    if (!to)
        to = "svg";
    var offset = viewportConvertWidth(vpname, gridSVGCoords[vpname].x,
                                      "svg", to);
    var width = viewportConvertWidth(vpname, x, from, to);
    return roundNumber(offset + width, 2);
};

/**
 * Returns a unit's y location relative to a viewport.
 *
 * @param {string} vpname The name of the viewport that the unit is drawn within
 * @param {number} x The size of the unit, based on 'from'
 * @param {string} from The input unit type
 * @param {string} to The output unit type (optional, defaults to "svg")
 * @returns {number} A unit in SVG pixels
 */
var viewportConvertY = function(vpname, x, from, to) {
    if (!to)
        to = "svg";
    var offset = viewportConvertHeight(vpname, gridSVGCoords[vpname].y,
                                       "svg", to);
    var height = viewportConvertHeight(vpname, x, from, to);
    return roundNumber(offset + height, 2);
};

/**
 * Converts from any unit to any other unit along the horizontal dimension, relative to a viewport.
 *
 * @param {string} vpname The name of the viewport that the unit is drawn within
 * @param {number} x The size of the unit, based on 'from'
 * @param {string} from The input unit type
 * @param {string} to The output unit type
 * @returns {number} A unit in SVG pixels
 */
var viewportConvertWidth = function(vpname, x, from, to) {
    var vpCoords = gridSVGCoords[vpname];
    var i = toInches(from, x,
                     vpCoords.width,
                     vpCoords.xscale,
                     vpCoords.inch);
    var u = toUnit(to, i,
                   vpCoords.width,
                   vpCoords.xscale,
                   vpCoords.inch);
    return roundNumber(u, 2);
};

/**
 * Converts from any unit to any other unit along the vertical dimension, relative to a viewport.
 *
 * @param {string} vpname The name of the viewport that the unit is drawn within
 * @param {number} x The size of the unit, based on 'from'
 * @param {string} from The input unit type
 * @param {string} to The output unit type
 * @returns {number} A unit in SVG pixels
 */
var viewportConvertHeight = function(vpname, x, from, to) {
    var vpCoords = gridSVGCoords[vpname];
    var i = toInches(from, x,
                     vpCoords.height,
                     vpCoords.yscale,
                     vpCoords.inch);
    var u = toUnit(to, i,
                   vpCoords.height,
                   vpCoords.yscale,
                   vpCoords.inch);
    return roundNumber(u, 2);
};

/**
 * Converts from any unit to what R thought were inches.
 *
 * @param {string} from The input unit type
 * @param {number} unitValue The size of the unit, based on 'from'
 * @param {number} vpDimSize The size of the viewport that the unit belongs in, in SVG pixels
 * @param {Array.<number>} nativeScale For the given dimension that we're converting along (x or y)
 * @param {number} dimInchSize The size of an inch along this dimension
 * @returns {number} The input unit in inches
 */
var toInches = function(from, unitValue, vpDimSize, nativeScale, dimInchSize) {
    var nativeToInches = function(nativeValue, nativeScale, vpDimSize, dimInchSize) {
        var dist = nativeValue - nativeScale[0];
        var nativeUnitSize = vpDimSize / Math.abs(nativeScale[1] - nativeScale[0]);
        return dist * nativeUnitSize / dimInchSize;
    };
    
    var npcToInches = function(npcValue, vpDimSize, dimInchSize) {
        return (npcValue * vpDimSize) / dimInchSize;
    };

    var result;
    switch (from) {
        case "native":
            result = nativeToInches(unitValue, nativeScale, vpDimSize, dimInchSize);
            break;
        case "npc":
            result = npcToInches(unitValue, vpDimSize, dimInchSize);
            break;
        case "inches":
            result = unitValue;
            break;
        case "cm":
            result = unitValue / 2.54;
            break;
        case "mm":
            result = unitValue / 25.4;
            break;
        case "points":
            result = unitValue / 72.27;
            break;
        case "picas":
            result = (unitValue * 12) / 72.27;
            break;
        case "bigpts":
            result = unitValue / 72;
            break;
        case "dida":
            result = unitValue / 1157 * 1238 / 72.27;
            break;
        case "cicero":
            result = unitValue * 12 / 1157 * 1238 / 72.27;
            break;
        case "scaledpts":
            result = unitValue / 65536 / 72.27;
            break;
        case "svg":
            result = unitValue / dimInchSize;
            break;
        default:
            console.error('Unsupported "from" unit: [%s]', from);
    }
    return result;
};

/**
 * Converts from what R thought were inches, to another unit.
 *
 * @param {string} to The desired output unit
 * @param {number} unitValue The source size in inches to be converted to another unit
 * @param {number} vpDimSize The size of the viewport that the unit belongs in, in SVG pixels
 * @param {Array.<number>} nativeScale For the given dimension that we're converting along (x or y)
 * @param {number} dimInchSize The size of an inch along this dimension
 * @returns {number} The input unit in 'to' units
 */
var toUnit = function(to, unitValue, vpDimSize, nativeScale, dimInchSize) {
    var inchesToNative = function(inchesValue, nativeScale, vpDimSize, dimInchSize) {
        var npc = (inchesValue * dimInchSize) / vpDimSize;
        var vpRange = nativeScale[1] - nativeScale[0];
        return (npc * vpRange) + nativeScale[0];
    };
    
    var inchesToNpc = function(inchesValue, vpDimSize, dimInchSize) {
        return (inchesValue * dimInchSize) / vpDimSize;
    };

    var result;
    switch (to) {
        case "native":
            result = inchesToNative(unitValue, nativeScale, vpDimSize, dimInchSize);
            break;
        case "npc":
            result = inchesToNpc(unitValue, vpDimSize, dimInchSize);
            break;
        case "inches":
            result = unitValue;
            break;
        case "cm":
            result = unitValue * 2.54;
            break;
        case "mm":
            result = unitValue * 25.4;
            break;
        case "points":
            result = unitValue * 72.27;
            break;
        case "picas":
            result = (unitValue / 12) * 72.27;
            break;
        case "bigpts":
            result = unitValue * 72;
            break;
        case "dida":
            result = unitValue * 1157 / 1238 * 72.27;
            break;
        case "cicero":
            result = unitValue / 12 * 1157 / 1238 * 72.27;
            break;
        case "scaledpts":
            result = unitValue * 65536 * 72.27;
            break;
        case "svg":
            result = unitValue * dimInchSize;
            break;
        default:
            console.error('Unsupported "to" unit: [%s]', to);
    }
    return result;
};

/**
 * Rounds a number to a specified amount of decimal places
 *
 * @param {number} number The input number to round
 * @param {number} digits The number of decimal places to round to
 * @returns {number} The rounded number
 */
var roundNumber = function(number, digits) {
    var multiple = Math.pow(10, digits);
    var rounded = Math.round(number * multiple) / multiple;
    return rounded;
};

/**
 * Returns the name of the viewport path that a grob belongs to.
 *
 * Note that this is going to find the first matching viewport path from
 * the list of element IDs up the tree. It may end up incorrectly returning
 * a grob name instead of a viewport path in the case where a grob has the
 * same name as a viewport path.
 *
 * @param {string} grobName The name of a grob
 * @returns {string} The unique path of the viewport that the grob belongs to
 */
var grobViewport = function(grobName) {
    var grob = document.getElementById(grobName);
    if (grob) {
        var foundViewport = false;
        var viewportPath;
        var grobParent = grob.parentNode;
        while (! foundViewport) {
            var vpPath = grob.getAttribute("id");
            var testVP = gridSVGCoords[vpPath];
            // If we have found a match in our VP coordinate list we
            // have a candidate viewport path, but a grob might have
            // the same name as a viewport path... 
            if (testVP) {
                viewportPath = vpPath;
                foundViewport = true;
            } else {
                grob = grobParent;
                grobParent = grob.parentNode;
            }
        }
        return viewportPath;
    } else {
        console.error("Unable to find grob [%s]", grobName);
    }
};

/**
 * Removes any empty text nodes from an XML tree.
 *
 * Often when we create XML, we use indentation to make the structure of
 * the XML document more obvious to someone who reads it.
 *
 * This is a good idea in general, but it makes parsing the DOM a bit
 * more challenging. Consider the following example:
 *
 * : <svg>
 * :     <rect ... />
 * : </svg>
 *
 * We would expect the <svg> node to have one child, when it in fact has
 * *three* child nodes. A text node, a rect node and a text node. This
 * goes against intuition so we would ideally like it so that only the 
 * nodes that contain elements, such as the rect node, remain. This is
 * the purpose of this function.
 * 
 * @param {Object} node An XML tree that we wish to prune
 */
var pruneTextNodes = function(node) {
    var blank = /^\s*$/;
    var child, next;
    switch (node.nodeType) {
        case 3: // Text node
            if (blank.test(node.nodeValue)) {
                node.parentNode.removeChild(node);
            }
            break;
        case 1: // Element node
        case 9: // Document node
            child = node.firstChild;
            while (child) {
                next = child.nextSibling;
                pruneTextNodes(child);
                child = next;
            }
            break;
    }
};

/**
 * Removes the numeric suffix from a viewport path.
 *
 * This function is useful in the case where we have a viewport path
 * because these paths are required to be unique. The reason for this is
 * because a viewport path can be used more than once.
 *
 * @param {string} vppath The modified and unique viewport path produced by gridSVG
 * @returns {string} The canonical viewport path, as known to grid
 */
var baseViewportPath = function(vppath) {
    var splitPath = vppath.split(".");
    // If there was actually something to split, get rid of the last value
    if (splitPath.length > 1) {
        splitPath.pop();
    }
    return splitPath.join(".");
};

/**
 * Escapes the colons present in a viewport path for use in selectors.
 *
 * Because the colon is a special character in CSS selectors, escape the
 * viewport path using this function first to ensure that the selector
 * will work as expected.
 *
 * @param {string} vppath The viewport path to escape.
 * @returns {string} An escaped viewport path, safe for use as a selector
 */
var escapeViewportPath = function(vppath) {
    return vppath.replace(/:/g, "\\:");
}

/**
 * Creates a URL for a GET/POST request. Automatically inserts separators
 * such as ?, & and =.
 *
 * @param {string} loc The location of the script to query.
 * @param {Object} params An object with keys representing GET/POST params,
 *                        and associated with their values.
 * @returns {string} The complete URL to request with.
 */
var queryBuilder = function(loc, params) {
    if (!params)
        return loc;

    var query = [];
    for (var k in params)
       query.push(encodeURIComponent(k) + "=" + encodeURIComponent(params[k]));
    var queryText = query.join("&"); 

    // If params is an object of length 0 we end up with ""
    if (!queryText)
        return loc;

    return loc + "?" + queryText;
}

// NOTE: The following code assumes that a global object, "gridSVGMappings"
//       is available. In other words, to use this code, load this object first.

/**
 * Returns the mapping from a known grob/viewport name to an SVG ID.
 * Assumes the variable 'gridSVGMappings' is in scope.
 *
 * @param {string} name The name of the object whose ID we are getting.
 * @param {string} type One of 'vp', 'grob' or 'ref'. Determines whether the name refers to a viewport or a grob or a reference to a defined object.
 * @param {string?} result One of 'id', 'selector' or 'xpath'. Determines the type of results we want back, i.e. SVG IDs, CSS selectors or XPath expressions.
 * @returns {Array} An array of values.
 *
 */
var getSVGMappings = function(name, type, result) {
    if (type !== "vp" && type !== "grob" && type !== "ref") {
        throw new Error("Invalid type specified. Must be one of 'vp', 'grob' or 'ref'.");
    }

    // Assume we want an ID by default, and not a selector/xpath
    if (! result) {
        result = "id";
    }

    type = type + "s";
    var obj = gridSVGMappings[type][name];
    if (! obj) {
        throw new Error("Name not found in mapping table.");
    }

    if (result === "id") {
        // Force suffix to be an array of suffixes because RJSONIO reduces
        // vectors to scalars if length(vec) == 1
        var suffix = obj.suffix;
        if (typeof suffix === "number") {
            suffix = [suffix];
        }
        var ids = [];
        for (var i = 0; i < suffix.length; i++) {
            ids.push(name + gridSVGMappings["id.sep"] + suffix[i]);
        }
        return ids;
    }
    if (result === "selector" || result === "xpath") {
        // Force results to be an array of results because RJSONIO reduces
        // vectors to scalars if length(vec) == 1
        var vals = obj[result];
        if (typeof vals === "string") {
            vals = [vals];
        }
        return vals;
    }
};

