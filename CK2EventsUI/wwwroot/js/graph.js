System.register(["dagre", "cytoscape", "cytoscape-qtip", "cytoscape-dagre", "cytoscape-navigator", "cytoscape-canvas", "handlebars"], function (exports_1, context_1) {
    "use strict";
    var __moduleName = context_1 && context_1.id;
    function main(data, triggers, options, pretties) {
        _data = data;
        _options = options;
        _pretty = pretties;
        cytoscape_qtip_1["default"](cytoscape_1["default"], $);
        cytoscape_dagre_1["default"](cytoscape_1["default"], dagre_1["default"]);
        cytoscape_canvas_1["default"](cytoscape_1["default"]);
        var nav = cytoscape_navigator_1["default"](cytoscape_1["default"], $);
        var cy = cytoscape_1["default"]({
            container: document.getElementById('cy'),
            style: [
                {
                    selector: 'node',
                    style: {
                        'label': 'data(label)'
                    }
                },
                {
                    selector: 'edge',
                    style: {
                        'width': 3,
                        'line-color': '#ccc',
                        'mid-target-arrow-color': '#ccc',
                        'mid-target-arrow-shape': 'triangle',
                        'curve-style': 'bezier'
                    }
                }
            ]
        });
        var roots = [];
        var qtipname = function (text) { return { content: text, position: { my: 'top center', at: 'bottom center' }, style: { classes: 'qtip-bootstrap', tip: { width: 16, height: 8 } }, show: { event: 'mouseover' }, hide: { event: 'mouseout' } }; };
        data.forEach(function (element) {
            var name;
            if (element.Comments.length > 0) {
                name = element.Comments[0];
                name = name.replace(new RegExp('#', 'g'), '');
                if (name.length > labelMaxLength) {
                    var endOfWord = name.indexOf(' ', labelMaxLength - 1);
                    var endOfWord = endOfWord === -1 ? name.length : endOfWord;
                    name = name.substring(0, endOfWord);
                }
            }
            else {
                name = element.ID;
            }
            var desc;
            if (element.Desc === '') {
                desc = element.ID;
            }
            else {
                desc = element.Desc;
            }
            var node = cy.add({ group: 'nodes', data: { id: element.ID, label: name, type: element.Key, hidden: element.Hidden } });
            node.qtip(qtipname(desc));
        });
        triggers.forEach(function (event) {
            var parentID = event[0];
            event[1].forEach(function (immediates) {
                immediates.forEach(function (target) {
                    var childID = target;
                    cy.add({ group: 'edges', data: { source: parentID, target: childID } });
                });
            });
        });
        options.forEach(function (event) {
            var parentID = event[0];
            event[1].forEach(function (option) {
                var optionName = option[0];
                option[1].forEach(function (target) {
                    if (cy.getElementById(target).length > 0) {
                        var edge = cy.add({ group: 'edges', data: { source: parentID, target: target } });
                        if (optionName !== "") {
                            edge[0].qtip(qtipname(optionName));
                        }
                    }
                    else {
                        cy.getElementById(parentID).data('deadend_option', true);
                    }
                });
            });
        });
        cy.fit();
        var layout = cy.layout({ name: 'dagre' });
        layout.run();
        cy.fit();
        var layer = cy.cyCanvas();
        var canvas = layer.getCanvas();
        var ctx = canvas.getContext('2d');
        cy.on("render", function (evt) {
            layer.resetTransform(ctx);
            layer.clear(ctx);
            layer.setTransform(ctx);
            ctx.shadowColor = "black";
            ctx.shadowBlur = 25 * cy.zoom();
            ctx.fillStyle = "#666";
            cy.nodes().forEach(function (node) {
                var text = node.data('type');
                var eventChars = text.split('_').map(function (f) { return f[0].toUpperCase(); }).join('');
                var eventChar = text[0].toUpperCase();
                var pos = node.position();
                ctx.fillStyle = node.data('hidden') ? "#EEE" : '#888';
                ctx.beginPath();
                ctx.arc(pos.x, pos.y, 15, 0, 2 * Math.PI, false);
                ctx.fill();
                ctx.fillStyle = "black";
                ctx.stroke();
                if (node.data('deadend_option')) {
                    ctx.arc(pos.x, pos.y, 13, 0, 2 * Math.PI, false);
                    ctx.stroke();
                }
                ctx.fillStyle = "black";
                ctx.font = "16px sans-serif";
                ctx.textAlign = "center";
                ctx.textBaseline = "middle";
                ctx.fillText(eventChars, pos.x, pos.y);
            });
            ctx.restore();
        });
        var defaults = {
            container: ".cy-row",
            viewLiveFramerate: 0,
            thumbnailEventFramerate: 30,
            thumbnailLiveFramerate: false,
            dblClickDelay: 200,
            removeCustomContainer: true,
            rerenderDelay: 100
        };
        cy.on('select', 'node', function (e) {
            var node = cy.$('node:selected');
            if (node.nonempty()) {
                showDetails(node.data('id'));
            }
        });
        cy.on('select', 'edge', function (e) {
            var edges = cy.edges('edge:selected');
            var edge = edges.first();
            var opts = {};
            opts.zoom = cy.zoom();
            opts.center = { eles: edge };
            cy.animate(opts);
        });
        cy.on("resize", function (e) {
            $("#cy").width(10);
            cy.resize();
            cy.center();
        });
    }
    function showDetails(id) {
        var node = _data.filter(function (x) { return x.ID === id; })[0];
        var pretty = _pretty.filter(function (x) { return x[0] === id; })[0][1];
        var options = _options.filter(function (x) { return x[0] === id; });
        var context = { title: node.ID, desc: node.Desc, full: pretty, options: options };
        var html = detailsTemplate(context);
        document.getElementById('detailsTarget').innerHTML = html;
    }
    exports_1("showDetails", showDetails);
    function go(file) {
        $.ajax({
            url: "GetData",
            data: { "file": file }
        })
            .done(function (data) {
            main(JSON.parse(data.item1), JSON.parse(data.item2), JSON.parse(data.item3), JSON.parse(data.item4));
        });
    }
    exports_1("go", go);
    var dagre_1, cytoscape_1, cytoscape_qtip_1, cytoscape_dagre_1, cytoscape_navigator_1, cytoscape_canvas_1, handlebars_1, labelMaxLength, _data, _options, _pretty, detailsTemplate;
    return {
        setters: [
            function (dagre_1_1) {
                dagre_1 = dagre_1_1;
            },
            function (cytoscape_1_1) {
                cytoscape_1 = cytoscape_1_1;
            },
            function (cytoscape_qtip_1_1) {
                cytoscape_qtip_1 = cytoscape_qtip_1_1;
            },
            function (cytoscape_dagre_1_1) {
                cytoscape_dagre_1 = cytoscape_dagre_1_1;
            },
            function (cytoscape_navigator_1_1) {
                cytoscape_navigator_1 = cytoscape_navigator_1_1;
            },
            function (cytoscape_canvas_1_1) {
                cytoscape_canvas_1 = cytoscape_canvas_1_1;
            },
            function (handlebars_1_1) {
                handlebars_1 = handlebars_1_1;
            }
        ],
        execute: function () {
            labelMaxLength = 30;
            detailsTemplate = handlebars_1["default"].compile("<h1>{{title}}</h1><div>{{desc}}</div><div><ul>{{#each options}}<li>{{this}}</li>{{/each}}</ul></div><pre>{{full}}</pre>");
        }
    };
});
