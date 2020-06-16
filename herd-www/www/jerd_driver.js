var debug;
var editors;
var currentState;
var dotcom = 'neato';

function initCurrentState() {
    debug = false;

    editors = {
        'bell' : null,
        'cat' : null,
        'cfg' : null,
        'herdoutput' : null,
        'litmus' : null,
        'record' : null,
    };

    currentState = {
        'record' : null,
        'displayName' : null,
        'shelf' : null,
        'compatNum' : null,
        'campaignCode' : [],
        'bell' : { 'origText' : null, 'origUrl' : null, 'pos' : null },
        'cat' : { 'origText' : null, 'origUrl' : null, 'pos' : null },
        'cfg' : { 'origText' : null, 'origUrl' : null, 'pos' : null },
        'litmus' : { 'origText' : null, 'origUrl' : null, 'pos' : null },
    };
}

function log(s) {
    if (debug) {
        console.log(s);
    }
}

function error(s) {
    console.log('ERROR: ' + s);
}

function basename(path) {
    return path.split('/').pop()
}

function shortname(path) {
    if (path === null) {
        return null
    }
    return basename(path).replace(/\.[^/.]+$/, "")
}

function indexOfShortFilenameInArray(name, array) {
    for (var i = 0; i < array.length; i++) {
        if (shortname(array[i]) === name) {
            return i;
        }
    }
    return -1
}

function findShortFilenameInArray(name, array) {
    var i = indexOfShortFilenameInArray(name, array);

    if (i >= 0) {
        return array[i];
    }
    return undefined;
}

function parseIntArg(s, base) {
    if (typeof s === 'undefined') {
        return undefined;
    }
    if (typeof base === 'undefined') {
        base = 10;
    }

    var x = parseInt(s, base);
    if (x === NaN) {
        return undefined;
    }
    return x;
}

function startsWith(s, prefix) {
    return s.substring(0, prefix.length) === prefix
}

function repeat(pattern, count) {
    if (count < 1) return '';
    var result = '';
    while (count > 1) {
        if (count & 1) result += pattern;
        count >>= 1, pattern += pattern;
    }
    return result + pattern;
}

function catalogueBaseUrlOfRecord(record) {
    return 'catalogue/' + record + '/'
}

function fileListOfEditorName(name) {
    return '#' + name + '-file-list'
}

function selectMenuIdOfEditorName(name) {
    return '#' + name + '-select'
}

function campaignRowIdOfKey(key) {
    return '#campaign-row-' + key.toString();
}

function campaignCheckboxIdOfKey(key) {
    return '#campaign-checkbox-' + key.toString();
}

function initEditor(name, theme, mode, readOnly, showGutter, onChange) {
    if (typeof readOnly === 'undefined') {
        readOnly = false;
    }
    if (typeof showGutter === 'undefined') {
        showGutter = true;
    }
    if (typeof onChange === 'undefined') {
        onChange = null;
    }

    var editor = ace.edit(name + '-editor');
    editors[name] = editor;
    editor.setTheme(theme);
    editor.getSession().setMode(mode);
    editor.setReadOnly(readOnly);
    editor.renderer.setShowGutter(showGutter);
    editor.$blockScrolling = Infinity;

    if (onChange !== null) {
        editor.on("change", (function(name, changeItem) {
            return function(e) {
                var itemText = $(changeItem).text();
                if (hasUnsavedContent(name)) {
                    if (itemText[itemText.length-1] !== "*") {
                        $(changeItem).html($(changeItem).text() + "*");
                    }
                } else {
                    if (itemText[itemText.length-1] === "*") {
                        $(changeItem).html(
                            $(changeItem).text()
                                .substring(0, itemText.length - 1)
                        );
                    }
                }
            }
        })(name, onChange));
    }
}

function setLitmusSyntaxHighlighting() {
    var editor = editors['litmus'];
    var text = editor.getValue();

    if (startsWith(text, 'C')) {
        editor.getSession().setMode('ace/mode/c_cpp');
    } else if (startsWith(text, 'LISA')) {
        // TODO: wrong
        editor.getSession().setMode('ace/mode/c_cpp');
    } else if (startsWith(text, 'ARM') || startsWith(text, 'AArch64')) {
        // TODO: wrong
        editor.getSession().setMode('ace/mode/assembly_x86');
    } else if (startsWith(text, 'PPC')) {
        // TODO: wrong
        editor.getSession().setMode('ace/mode/assembly_x86');
    } else if (startsWith(text, 'GPU_PTX')) {
        // TODO: wrong
        editor.getSession().setMode('ace/mode/assembly_x86');
    } else {
        // default
        log('could not find suitable syntax highlighting for litmus test');
        editor.getSession().setMode('ace/mode/c_cpp');
    }
}

function clearCatEditor() {
    editors['cat'].setValue('"I can\'t dance"\n\n',-1);
}

function clearBellEditor() {
    editors['bell'].setValue('',-1);
}

function clearLitmusEditor() {
    editors['litmus'].setValue('',-1);
}

function clearCfgEditor() {
    // default is web.cfg. Watch out for this getting out of sync...
    editors['cfg'].setValue(
        'graph columns\n'
            + 'squished true\n'
            + 'showevents memory\n'
            + 'showinitwrites false\n'
            + 'fontsize 8\n'
            + 'scale 0.75\n'
            + 'xscale 1.0\n'
            + 'yscale 0.6667\n'
            + 'arrowsize 0.5\n'
            + 'doshow fr\n'
            + 'showinitrf false\n'
            + 'showfinalrf false\n'
            + 'splines spline\n'
            + 'pad 0.1\n'
            + 'showlegend false\n',
            -1);
}

function clearAllEditors() {
    clearCatEditor();
    clearBellEditor();
    clearLitmusEditor();
    clearCfgEditor();
}

function populateEditorPanelHeader(id, elems) {
    $(id).empty();

    elemsPerColumn = 20;
    nColumns = Math.ceil(elems.length / elemsPerColumn);

    if (nColumns > 4) {
        // no more than 4 columns
        // we cannot have 5 columns because 5 is not a divisor of 12
        nColumns = 4;
    }

    // adjust balancing, in particular to take care of special cases above
    // but also it looks nicer
    elemsPerColumn = Math.ceil(elems.length / nColumns);

    colmd = (12 / nColumns) | 0;
    $(id).css('width', (400 * nColumns).toString());

    $.each(elems, function(key, val) {
        if (key % elemsPerColumn === 0) {
            ul = $('<ul/>')
                .addClass('list-unstyled')
                .addClass('col-md-'+colmd.toString())
            // 380px < 400px this is a hack so that nColumns fit
            // inside the box. For some reason the width of a column
            // ends up as slightly more than the number here so I gave
            // 20px of headroom #yolo
                .css('width', '380px')
                .appendTo(id);
        }

        val.appendTo(ul);
    });
}

function prepareEditorFile(editorName, key, val, callback) {
    var baseVal = basename(val);
    var itemId = 'file-' + baseVal.replace(/\+/g, '.');

    return $('<li/>')
        .append($('<a/>', {
            text: baseVal,
            id: itemId,
            href: '#',
        }).click(
            (function(editorName, key, val, callback) {
                return function(e) {
                    if (callback !== null) {
                        callback(key, val, editorName, e);
                    } else {
                        downloadAndSetEditorValue(val, editorName, key);
                    }

                    e.preventDefault();
                };
            })(editorName, key, val, callback)
        ))
}

function prepareItemListWithCompatibilities(
    editorName,
    compatString, compatElems,
    incompatString, incompatElems,
    callback) {
    var result = [];

    if (compatString !== null) {
        var item = $('<li/>')
            .addClass("dropdown-header")
            .append(compatString);
        result.push(item);
    }

    if (compatElems !== null) {
        $.each(compatElems, function(key, val) {
            result.push(prepareEditorFile(editorName, key, val, callback));
        });
    }

    if (incompatString !== null) {
        result.push(
            $('<li/>').addClass("divider")
        );
        result.push(
            $('<li/>')
                .addClass("dropdown-header")
                .append(incompatString)
        );
    }

    if (incompatElems !== null) {
        $.each(incompatElems, function(key, val) {
            result.push(prepareEditorFile(editorName, key, val, callback));
        });
    }

    return result
}

function doDownloadAndSetEditorValue(url, name, pos) {
    log('getting ' + name + ': ' + url);

    $.ajax({
        url: url,
        success: (function(url, name, pos) {
            return function (data) {
                currentState[name]['origUrl'] = url;
                currentState[name]['origText'] = data;
                currentState[name]['pos'] = pos;
                updateLinkToExample();

                $(selectMenuIdOfEditorName(name)).html(basename(url));

                editors[name].setValue(data, -1);
                if (name === 'litmus') {
                    setLitmusSyntaxHighlighting();
                }
                if (name == 'cat') {
                    var nhref = 'weblib/' + basename(url) + '.html' ;
                    document.getElementById('show-cat').href = nhref ;
                }
            }
        })(url, name, pos)
    });
}

function hasUnsavedContent(name) {
    return currentState[name]['origText'] !== null
        && !(editors[name].getValue() === currentState[name]['origText'])
}

function downloadAndSetEditorValue(url, name, pos, warnOnChange) {
    if (typeof warnOnChange === 'undefined') {
        warnOnChange = true;
    }

    if (warnOnChange && hasUnsavedContent(name)) {
        log('downloadAndSetEditorValue thinks '+name+' has changed');
        $("#unsavedModalProceed").unbind("click");
        $("#unsavedModalProceed").click(
            (function(url, name, pos) {
                return function(e) {
                    doDownloadAndSetEditorValue(url, name, pos);
                };
            })(url, name, pos));
        $("#unsavedModalText").html(name + ' has unsaved content');
        $("#unsavedModal").modal();
    } else {
        doDownloadAndSetEditorValue(url, name, pos);
    }
}


function updateEditor(
    name,
    compatString, compatElems,
    incompatString, incompatElems,
    compatKey,
    callback) {
    var items = prepareItemListWithCompatibilities(
        name,
        compatString, compatElems,
        incompatString, incompatElems,
        callback);

    populateEditorPanelHeader(fileListOfEditorName(name), items);

    newCurrentElement = compatElems[compatKey];

    if (currentState[name]['origUrl'] !== newCurrentElement) {
        downloadAndSetEditorValue(newCurrentElement, name, compatKey, false);
    }
}

function compatibilityClasses(name, type) {
    return compatibilities.filter(function (compatClass) {
        return compatClass[type].indexOf(name) >= 0;
    });
}

function compatibilityOfInput(inputName, inputType, dualType, dualCurrentUrl) {
    var compatibility;

    var compatClasses = currentState['shelf']['compatibilities']
        .filter(function (compatClass) {
            return compatClass[inputType].indexOf(inputName) >= 0
                || indexOfShortFilenameInArray(inputName, compatClass[inputType]) >= 0;
        });

    if (compatClasses.length === 0) {
        error('no compatibility class for '+inputName);
        return null;
    }

    var dualCompatClasses = compatClasses.filter(function (compatClass) {
        return compatClass[dualType].indexOf(dualCurrentUrl) >= 0;
    });

    if (dualCompatClasses.length > 0) {
        if (dualCompatClasses.length > 1) {
            error('more than one entries in compatibilities for (' + inputName + ',' + dualCurrentUrl + ')');
        }
        compatibility = dualCompatClasses[0];
    } else {
        compatibility = compatClasses[0];
    }
    return compatibility;
}


function doUpdateAllEditorsWithCompatibility(
    compatNum,
    compatCats, compatBells, compatLitmuses,
    catNum, bellNum, litmusNum) {
    var incompatCats = currentState['shelf']['cats'].filter(function (i) {
        return compatCats.indexOf(i) < 0
    });
    var bellUrl = currentState['shelf']['compatibilities'][compatNum]['bells'][bellNum];
    var compatCatsString = "compatible with " + basename(bellUrl);
    var incompatCatsString = "other cats";
    var catCallback = (function(compatNum) {
        return function(key, val, editorName, e) {
            updateAllEditorsWithCompatibility(undefined, val);
        };
    })(compatNum);

    var incompatBells = currentState['shelf']['bells'].filter(function (i) {
        return compatBells.indexOf(i) < 0
    });
    var catUrl = currentState['shelf']['compatibilities'][compatNum]['cats'][catNum];
    var compatBellsString = "compatible with " + basename(catUrl);
    var incompatBellsString = "other bells";
    var bellCallback = (function(compatNum) {
        return function(key, val, editorName, e) {
            updateAllEditorsWithCompatibility(undefined, undefined, val);
        };
    })(compatNum);

    updateEditor(
        'cat',
        compatCatsString, compatCats,
        incompatCatsString, incompatCats,
        catNum,
        catCallback);

    $('#bell-panel').show();
    updateEditor(
        'bell',
        compatBellsString, compatBells,
        incompatBellsString, incompatBells,
        bellNum,
        bellCallback);

    updateEditor(
        'litmus',
        null, compatLitmuses,
        null, null,
        litmusNum,
        null);

    currentState['compatNum'] = compatNum;
}

function updateAllEditorsWithCompatibility(compatNum, cat, bell, litmus, warnOnChange) {
    if (typeof warnOnChange === 'undefined') {
        warnOnChange = true;
    }

    var compatibilities = currentState['shelf']['compatibilities'];
    if (typeof compatNum === 'undefined') {
        if (typeof cat === 'undefined' && typeof bell === 'undefined') {
            compatNum = 0;
        } else if (typeof cat === 'string') {
            compatNum = compatibilities.indexOf(compatibilityOfInput(
                cat, 'cats', 'bells', currentState['bell']['origUrl']));
        } else if (typeof bell === 'string') {
            compatNum = compatibilities.indexOf(compatibilityOfInput(
                bell, 'bells', 'cats', currentState['cat']['origUrl']));
        }
    }

    var compatibility = currentState['shelf']['compatibilities'][compatNum];
    var compatCats = compatibility['cats'];
    var compatBells = compatibility['bells'];
    var compatLitmuses = compatibility['litmuses'];
    var catNum, bellNum, litmusNum;

    // try to keep the current cat/bell/litmus if they are still compatible
    if (typeof cat === 'undefined') {
        if (compatCats.indexOf(currentState['cat']['origUrl']) >= 0) {
            catNum = compatCats.indexOf(currentState['cat']['origUrl']);
        } else {
            catNum = 0;
        }
    } else if (typeof cat === 'number') {
        catNum = cat;
    } else {
        // cat is the short name or the url of the cat file
        if (typeof cat === 'string' && compatCats.indexOf(cat) < 0) {
            catNum = indexOfShortFilenameInArray(cat, compatCats);
        } else {
            catNum = compatCats.indexOf(cat);
        }
    }

    if (typeof bell === 'undefined') {
        if (compatBells.indexOf(currentState['bell']['origUrl']) >= 0) {
            bellNum = compatBells.indexOf(currentState['bell']['origUrl']);
        } else {
            bellNum = 0;
        }
    } else if (typeof bell === 'number') {
        bellNum = bell;
    } else {
        // bell is the short name or the url of the bell file
        if (typeof bell === 'string' && compatBells.indexOf(bell) < 0) {
            bellNum = indexOfShortFilenameInArray(bell, compatBells);
        } else {
            bellNum = compatBells.indexOf(bell);
        }
    }

    if (typeof litmus === 'undefined') {
        if (compatLitmuses.indexOf(currentState['litmus']['origUrl']) >= 0) {
            litmusNum = compatLitmuses.indexOf(currentState['litmus']['origUrl']);
        } else {
            litmusNum = 0;
        }
    } else if (typeof litmus === 'number') {
        litmusNum = litmus;
    } else {
        // litmus is the short name or the url of the litmus file
        if (typeof litmus === 'string' && compatLitmuses.indexOf(litmus) < 0) {
            litmusNum = indexOfShortFilenameInArray(litmus, compatLitmuses);
        } else {
            litmusNum = compatLitmuses.indexOf(litmus);
        }
    }

    var changed = '';
    var pluralHas = 'has';
    if (compatCats[catNum] !== currentState['cat']['origUrl']
        && hasUnsavedContent('cat')) {
        changed += 'cat';
    }
    if (compatBells[bellNum] !== currentState['bell']['origUrl']
        && hasUnsavedContent('bell')) {
        if (changed !== '') {
            changed += ', ';
            pluralHas = 'have';
        }
        changed += 'bell';
    }
    if (compatLitmuses[litmusNum] !== currentState['litmus']['origUrl']
        && hasUnsavedContent('litmus')) {
        if (changed !== '') {
            changed += ', ';
            pluralHas = 'have';
        }
        changed += 'litmus';
    }

    if (warnOnChange && changed.length > 0) {
        log('updateAllEditorsWithCompatibility thinks '+changed+' has changed');
        $("#unsavedModalProceed").unbind("click");
        $("#unsavedModalProceed").click(
            (function(
                compatNum,
                compatCats, compatBells, compatLitmuses,
                catNum, bellNum, litmusNum) {
                return function(e) {
                    doUpdateAllEditorsWithCompatibility(
                        compatNum,
                        compatCats, compatBells, compatLitmuses,
                        catNum, bellNum, litmusNum);
                };
            })(compatNum,
               compatCats, compatBells, compatLitmuses,
               catNum, bellNum, litmusNum));
        $("#unsavedModalText").html(changed + ' ' + pluralHas + ' unsaved content');
        $("#unsavedModal").modal();
    } else {
        doUpdateAllEditorsWithCompatibility(
            compatNum,
            compatCats, compatBells, compatLitmuses,
            catNum, bellNum, litmusNum);
    }
}

function updateAllEditors(compatNum, bellKey, catKey, cfgKey, litmusKey, warnOnChange) {
    if (typeof warnOnChange === 'undefined') {
        warnOnChange = true;
    }

    var pythonShelfUrl = catalogueBaseUrlOfRecord(currentState['record']) + "shelf.py";
    log('getting: ' + pythonShelfUrl);
    $.ajax({
        url: pythonShelfUrl,
        success: function (data) {
            editors['record'].setValue(data, -1);
        }
    });

    if (typeof cfgNum === 'undefined') {
        cfgNum = 0;
    }

    updateEditor(
        'cfg',
        null, currentState['shelf']['cfgs'],
        null, null,
        cfgNum,
        null);

    if (currentState['shelf']['compatibilities'] !== null) {
        updateAllEditorsWithCompatibility(
            compatNum,
            catKey,
            bellKey,
            litmusKey,
            warnOnChange);
    } else {
        currentState['compatNum'] = null;

        var bellNum, catNum, litmusNum;
        if (typeof bellKey === 'undefined') {
            bellNum = 0;
        } else {
            bellNum = indexOfShortFilenameInArray(bellKey, currentState['shelf']['bells']);
        }
        if (typeof catKey === 'undefined') {
            catNum = 0;
        } else {
            catNum = indexOfShortFilenameInArray(catKey, currentState['shelf']['cats']);
        }
        if (typeof litmusKey === 'undefined') {
            litmusNum = 0;
        } else {
            litmusNum = indexOfShortFilenameInArray(litmusKey, currentState['shelf']['illustrative_tests']);
        }

        updateEditor(
            'cat',
            null, currentState['shelf']['cats'],
            null, null,
            catNum,
            null);

        // bell files are optional
        if (currentState['shelf']['bells'] !== null) {
            $('#bell-panel').show();
            updateEditor(
                'bell',
                null, currentState['shelf']['bells'],
                null, null,
                bellNum,
                null);
        } else {
            $('#bell-panel').hide();
            clearBellEditor();
            currentState['bell']['origUrl'] = null;
            currentState['bell']['origText'] = null;
            currentState['bell']['pos'] = null;
        }

        updateEditor(
            'litmus',
            null, currentState['shelf']['illustrative_tests'],
            null, null,
            litmusNum,
            null);
    }
}

function resolveUrlsInShelf(shelf, record) {
    function resolveUrls(val, index, a) {
        return catalogueBaseUrlOfRecord(record) + val;
    }

    $.each(['cats', 'bells', 'illustrative_tests', 'cfgs'], function(key, val) {
        if (shelf[val] !== null) {
            shelf[val] = shelf[val].map(resolveUrls);
        }
    });

    if (shelf['compatibilities'] !== null) {
        $.each(shelf['compatibilities'], function (index, compat) {
            $.each(['cats', 'bells', 'litmuses'], function(key, val) {
                shelf['compatibilities'][index][val] = shelf['compatibilities'][index][val].map(resolveUrls);
            });
        });
    }

    return shelf;
}

function loadCampaign(testArray, campaignCode) {
    if (typeof campaignCode === 'undefined') {
        campaignCode = [];
        for (var index = 0; index < (testArray.length  / 4) | 0; index++) {
            campaignCode.push(0);
        }
    }

    var campaignTable = $('#campaign-table');
    campaignTable.empty();

    var headerRow = $('<tr/>')
        .append($('<th/>', {
            text: 'forbidden',
        }))
        .append($('<th/>', {
            text: 'litmus test',
        }));
    campaignTable.append(headerRow);

    var addCampaignRow = function(key, val) {
        var baseVal = basename(val);
        var campaignInt = campaignCode[(key/4) | 0];
        var isRowChecked = (campaignInt & (1 << (key % 4))) !== 0;
        if (typeof campaignInt === 'undefined') {
            error('ill-formed campaign argument');
            campaignInt = 0;
        }

        var row = $('<tr/>', {id: campaignRowIdOfKey(key).substring(1)})
            .append($('<td/>')
                    .append($('<input/>', {
                        type: "checkbox",
                        id: campaignCheckboxIdOfKey(key).substring(1),
                        ariaLabel: 'is ' + baseVal + ' allowed',
                    })
                            .prop('checked', isRowChecked)
                            .click((function (key) {
                                return function(e) {
                                    var offset = (key/4) | 0;

                                    if ($(this).is(':checked')) {
                                        currentState['campaignCode'][offset] =
                                            (currentState['campaignCode'][offset] | (1 << (key%4))) %16;
                                    } else {
                                        currentState['campaignCode'][offset] =
                                            (currentState['campaignCode'][offset] & (~(1 << (key%4)))) %16;
                                    }
                                    updateLinkToExample();
                                }
                            })(key))
                           ))
            .append($('<td/>', {
                html: $('<a/>', {
                    href: '#',
                    text: baseVal,
                })
                .click((function (key, val) {
                    return function(e) {
                        toggleLitmusCampaign(true);
                        downloadAndSetEditorValue(val, 'litmus', key, true);
                    }
                })(key, val)),
            }));

        campaignTable.append(row);
    };

    $.each(testArray, addCampaignRow);
    currentState['campaignCode'] = campaignCode;
}


function readRecord(record, displayName, compatNum, bellKey, catKey, cfgKey, litmusKey, campaignCode) {
    if (typeof displayName === 'undefined') {
        displayName = record;
    }

    if (record === currentState['record']
        && typeof bellString === 'undefined' && typeof catString === 'undefined'
        && typeof cfgString === 'undefined' && typeof litmusString === 'undefined') {
        return;
    }

    var shelfUrl = catalogueBaseUrlOfRecord(record) + "shelf.json";

    $.getJSON(shelfUrl, function(result){
        var loadRecord = (function (record, displayName, compatNum, bellKey, catKey, cfgKey, litmusKey, campaignCode, shelfText) {
            return function (e) {
                clearAllEditors();
                resetOutputs();
                toggleLitmusCampaign(true);

                currentState['record'] = record;
                currentState['displayName'] = displayName;
                currentState['shelf'] = resolveUrlsInShelf(result, record);

                updateAllEditors(compatNum, bellKey, catKey, cfgKey, litmusKey, false);

                if (currentState['shelf']['compatibilities'] === null) {
                    loadCampaign(currentState['shelf']['illustrative_tests'], campaignCode);
                } else {
                    loadCampaign(currentState['shelf']['compatibilities'][currentState['compatNum']]['litmuses'], campaignCode);
                }

                $('#record-select').html(displayName);
            }
        })(record, displayName, compatNum, bellKey, catKey, cfgKey, litmusKey, campaignCode, result);

        if (hasUnsavedContent('cat')
            || hasUnsavedContent('bell')
            || hasUnsavedContent('litmus')
            || hasUnsavedContent('cfg')
           ) {
            log('readRecord thinks something has changed');
            $("#unsavedModalProceed").unbind("click");
            $("#unsavedModalProceed").click(loadRecord);
            $("#unsavedModalText").html('You have unsaved content');
            $("#unsavedModal").modal();
        } else {
            loadRecord();
        }
    });
}

function display_dot(i, dot) {
    try {
        options = [];
        if (!debug) {
            // TODO: the quiet, it does nothing!
            options.push('-q');
        }
        svg_elem = Viz(dot, "svg", dotcom, options);
    } catch(e) {
        log('failed to load dot' + i.toString() + ': ' + dot);
    }

    li = $('<li/>')
        .attr('data-target', '#dotCarousel')
        .attr('data-slide-to', i.toString());
    if (i === 0) {
        li.addClass('active');
    }
    indicators = $("#dotIndicators");
    li.appendTo(indicators);

    div = $('<div/>')
        .addClass('item')
        .addClass('text-center')
        .append(svg_elem);
    if (i === 0) {
        div.addClass('active');
    }
    /// caption is useless for now (always the same name)
    // caption_div = $('<div/>')
    //     .addClass("carousel-caption")
    //     .appendTo(div);
    // caption = $('<h3/>')
    //     .html(current_dot_name)
    //     .appendTo(caption_div);
    inner = $("#dotInner");
    div.appendTo(inner);
}

function trim(s){
    return ( s || '' ).replace( /^\s+|\s+$/g, '' );
}

function push_current_dot() {
    var dot = trim(current_dot)
    if (dot === '') {
        return;
    }
    dot_outputs.push(dot);
}

function herd_stderr(s) {
    window.alert(s);
}

function herd_output(s) {
    log(s);
    var end_dot = s.match(/^DOTEND /);
    var digraph = s.match(/^digraph /);
    var start_dot = s.match(/^DOTBEGIN /);
    var com_dot = s.match(/^DOTCOM /);

    if (end_dot !== null) {
        log('end of dot processing')
        push_current_dot();
        current_dot = null;
    } else if (start_dot !== null) {
        log('start of dot processing')
        push_current_dot();
        current_dot = '';
    } else if (com_dot != null) {
        dotcom =  s.split(' ').pop().split('\n').shift();
        log('from <' + s + '> dotcom is <' + dotcom + '>')
    } else if (digraph !== null) {
        log('new digraph')
        push_current_dot()
        current_dot = s;
        current_dot += '  graph [ dpi = 150 ];\n'
    } else if (current_dot !== null) {
        current_dot += s;
    } else {
        editors['herdoutput'].insert(s);
    }
}

function showOutputs() {
    $("#herd-outputs").show();
}

function resetOutputs() {
    editors['herdoutput'].setValue('');
    $("#herd-outputs").hide();

    current_dot_name = null
    current_dot = null
    dot_outputs = []

    $("#dotIndicators").empty();
    $("#dotInner").empty();
    document.getElementById("carouselControls").style.display = 'none';
}

function displayDotOutputs() {
    $.each(dot_outputs, display_dot);

    if (dot_outputs.length > 0) {
        document.getElementById("carouselControls").style.display = 'block';
    }
}

function jerdIt() {
    resetOutputs();
    showOutputs();
    var bellStr = editors['bell'].getValue();
    var catStr = editors['cat'].getValue();
    var cfgStr = editors['cfg'].getValue();
    var litmusStr = editors['litmus'].getValue();
    runHerd(
        bellStr,
        catStr,
        litmusStr,
        cfgStr,
    );
    displayDotOutputs();
}

function jerdAll() {
    $.each(currentState['shelf']['illustrative_tests'], function(key, val) {
        $.ajax({
            url: val,
            success: function (data) {
                resetOutputs();

                var bellStr = editors['bell'].getValue();
                var cfgStr = editors['cfg'].getValue();
                var catStr = editors['cat'].getValue();
                var litmusStr = data;

                runHerd(
                    bellStr,
                    catStr,
                    litmusStr,
                    cfgStr,
                );

                var herdOutput = editors['herdoutput'].getValue();
                if (herdOutput === '') {
                    $(campaignRowIdOfKey(key)).css('background-color', '#aaaaaa');
                } else if (isHerdOutputForbidden(herdOutput) !== $(campaignCheckboxIdOfKey(key)).is(':checked')) {
                    $(campaignRowIdOfKey(key)).css('background-color', '#ff7777');
                } else {
                    $(campaignRowIdOfKey(key)).css('background-color', '#77ff77');
                }
            }
        });
    });
}

function isHerdOutputForbidden(s) {
    return /^No$/m.test(s)
}

function toggleLitmusCampaign(forceOff) {
    if (typeof forceOff === 'undefined') {
        forceOff = false;
    }

    if (forceOff
        && document.getElementById('jerdall-button').style.display !== 'none') {
        // user hasn't clicked the button, need to simulate
        $("#litmus-campaign").button('toggle');
    }

    if (!forceOff
        && document.getElementById('jerdall-button').style.display === 'none') {
        document.getElementById('litmus-panel-body').style.display = 'none';
        document.getElementById('jerdit-button').style.display = 'none';
        document.getElementById('jerdall-button').style.display = 'block';
        document.getElementById('campaign-table').style.display = 'block';
    } else {
        document.getElementById('litmus-panel-body').style.display = 'block';
        document.getElementById('jerdit-button').style.display = 'block';
        document.getElementById('jerdall-button').style.display = 'none';
        document.getElementById('campaign-table').style.display = 'none';
    }
}

function setDisplayOfEditor(name, display) {
    document.getElementById(name + '-editor').style.display = display;
    document.getElementById(name + '-editor-buttons').style.display = display;
    document.getElementById(name + '-select-menu').style.display = display;
}

function hideEditor(name) {
    setDisplayOfEditor(name, 'none');
}

function showEditor(name) {
    setDisplayOfEditor(name, 'block');
    editors[name].renderer.updateFull();
}

function toggleCfg(forceOff) {
    if (typeof forceOff === 'undefined') {
        forceOff = false;
    }

    if (document.getElementById('cat-editor').style.display === 'none') {
        showEditor('cat');
        hideEditor('cfg');
    } else {
        hideEditor('cat');
        showEditor('cfg');
    }
}

function updateOnCollapseIn(name, id) {
    $(id).on('show.bs.collapse', (function(name) {
        return function (e) {
            editors[name].renderer.updateFull();
        }
    })(name));
}

function wireEditorButtons(name) {

    $('#'+name+'-erase').click(
        (function(name) {
            return function(e) {
                var editor = editors[name];
                editor.setValue('', -1);
                this.blur();
                editor.focus();
            };
        })(name)
    );

    $('#'+name+'-reset').click(
        (function(name) {
            return function(e) {
                var editor = editors[name];
                if (currentState[name]['origText'] !== null) {
                    editor.setValue(currentState[name]['origText'], -1);
                }
                this.blur();
                editor.focus();
            };
        })(name)
    );

    $('#'+name+'-undo').click(
        (function(name) {
            return function(e) {
                var editor = editors[name];
                editor.undo();
                this.blur();
                editor.focus();
            };
        })(name)
    );
}

function saveCurrentES() {
    var carouselData = $('#dotCarousel').data('bs.carousel');
    var currentIndex = carouselData.getItemIndex(carouselData.$element.find('.item.active'));
    var currentDot = dot_outputs[currentIndex];
    var svg = Viz(currentDot, "svg", dotcom, options);
    var blob = new Blob([svg], {type: "text/plain;charset=utf-8"});
    saveAs(blob, "es" + currentIndex.toString() + ".svg");
}

$(function () {
    initCurrentState();

    // bootstrap initialisation
    $('[data-toggle="popover"]').popover();
    $('[data-toggle="tooltip"]').tooltip();

    // ace initialisation
    initEditor("bell", "ace/theme/chrome", "ace/mode/ocaml", false, true, selectMenuIdOfEditorName("bell"));
    initEditor("cat", "ace/theme/chrome", "ace/mode/ocaml", false, true, selectMenuIdOfEditorName("cat"));
    editors['cat'].setOptions({
        enableLinking: true
    });
    editors['cat'].on("linkClick",function(data){
        if (debug) console.log("CLICK", data); // Just for testing so you can see what classes things are...
  if(data && data.token && data.token.type == "string"){
      window.open("weblib/" + data.token.value.replace(/^"/,'').replace(/"$/,''), "_blank");
  }
 });
    initEditor("cfg", "ace/theme/chrome", "ace/mode/plain_text", false, true, selectMenuIdOfEditorName("cfg"));
    initEditor("herdoutput", "ace/theme/chrome", "ace/mode/plain_text", true, false);
    editors['herdoutput'].setOptions({readOnly: true, highlightActiveLine: false, highlightGutterLine: false});
    editors['herdoutput'].renderer.$cursorLayer.element.style.display = "none"
    initEditor("litmus", "ace/theme/chrome", "ace/mode/c_cpp", false, true, selectMenuIdOfEditorName("litmus"));
    initEditor("record", "ace/theme/chrome", "ace/mode/python", true, true);
    hideEditor('cfg');
    wireEditorButtons('bell');
    wireEditorButtons('cat');
    wireEditorButtons('cfg');
    wireEditorButtons('litmus');

    // workaround to update hidden editors
    updateOnCollapseIn('cat', '#cat-collapse');
    updateOnCollapseIn('bell', '#bell-collapse');
    updateOnCollapseIn('record', '#record-collapse');

    String.prototype.hashCode = function() {
        var hash = 0, i, chr, len;
        if (this.length === 0) return hash;
        for (i = 0, len = this.length; i < len; i++) {
            chr   = this.charCodeAt(i);
            hash  = ((hash << 5) - hash) + chr;
            hash |= 0; // Convert to 32bit integer
        }
        return hash;
    };

    var record = $.getQueryString('record'),
        displayName = $.getQueryString('name'),
        compatNum = parseIntArg($.getQueryString('compatClass')),
        bellString = $.getQueryString('bell'),
        catString = $.getQueryString('cat'),
        cfgString = $.getQueryString('cfg'),
        litmusString = $.getQueryString('litmus'),
        campaignCodeString = $.getQueryString('campaign'),
        campaignCode = undefined;

    if (typeof campaignCodeString !== 'undefined') {
        campaignCode = [];
        for (var index = campaignCodeString.length - 1; index >= 0; index--) {
            campaignCode.push(parseIntArg(campaignCodeString.charAt(index), 16));
        }
    }

    if (typeof record === 'undefined') {
        record = 'aarch64';
    }
    readRecord(record, displayName, compatNum, bellString, catString, cfgString, litmusString, campaignCode);
    current_dot_name = null;
    current_dot = null;
    total_dots = 0;
})

function updateLinkToExample() {
    var pathname;
    if (basename(location.pathname) === '') {
        pathname = location.pathname + 'index.html';
    } else {
        pathname = location.pathname;
    }

    var url = [location.protocol, '//', location.host, pathname].join('');


    url += '?record=' + encodeURIComponent(currentState['record']);
    if (currentState['displayName'] !== currentState['record']) {
        url += '&name=' + encodeURIComponent(currentState['displayName']);
    }
    if (currentState['shelf']['compatibilities'] !== null) {
        url += '&compatClass=' + currentState['compatNum'].toString();
    }
    if (currentState['bell']['origUrl'] !== null) {
        url += '&bell=' + encodeURIComponent(shortname(currentState['bell']['origUrl']));
    }
    if (currentState['cat']['origUrl'] !== null) {
        url += '&cat=' + encodeURIComponent(shortname(currentState['cat']['origUrl']));
    }
    if (currentState['litmus']['origUrl'] !== null) {
        url += '&litmus=' + encodeURIComponent(shortname(currentState['litmus']['origUrl']));
    }
    if (currentState['cfg']['origUrl'] !== null) {
        url += '&cfg=' + encodeURIComponent(shortname(currentState['cfg']['origUrl']));
    }

    campaignCode = '';
    existsCampaign = false;
    for (var index = currentState['campaignCode'].length -1; index >= 0; index--) {
        if (currentState['campaignCode'][index] !== 0) {
            existsCampaign = true;
        }
        campaignCode += currentState['campaignCode'][index].toString(16);
    }
    if (existsCampaign) {
        url += '&campaign=' + campaignCode;
    }

    $('#linkify').attr('data-content', '<a href="' + url + '">' + url + '</a>');
}

;(function ($) {
    $.extend({
        getQueryString: function (name) {
            function parseParams() {
                var params = {},
                    e,
                    // a = /\+/g,  // Regex for replacing addition symbol with a space
                    r = /([^&=]+)=?([^&]*)/g,
                    d = function (s) {
                        return decodeURIComponent(s);
                        //return decodeURIComponent(s.replace(a, " "));
                    },
                    q = window.location.search.substring(1);

                while (e = r.exec(q))
                    params[d(e[1])] = d(e[2]);

                return params;
            }

            if (!this.queryStringParams)
                this.queryStringParams = parseParams();

            return this.queryStringParams[name];
        }
    });
})(jQuery);
