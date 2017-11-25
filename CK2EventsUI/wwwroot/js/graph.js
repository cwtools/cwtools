System.register(["dagre", "cytoscape", "cytoscape-qtip", "cytoscape-dagre", "cytoscape-navigator", "cytoscape-canvas"], function (exports_1, context_1) {
    "use strict";
    var __moduleName = context_1 && context_1.id;
    function sayHello() {
        var compiler = document.getElementById("compiler").value;
        var framework = document.getElementById("framework").value;
        return "Hello from " + compiler + " and " + framework + "!";
    }
    function main(data, triggers, options) {
        cytoscape_qtip_1["default"](cytoscape_1["default"], $);
        cytoscape_dagre_1["default"](cytoscape_1["default"], dagre_1["default"]);
        cytoscape_canvas_1["default"](cytoscape_1["default"]);
        var nav = cytoscape_navigator_1["default"](cytoscape_1["default"]);
        var cy = cytoscape_1["default"]({
            container: document.getElementById('cy'),
            style: [
                {
                    selector: 'node',
                    style: {
                        'label': 'data(id)'
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
            var name = element.ID;
            var desc;
            if (element.Desc === '') {
                desc = element.ID;
            }
            else {
                desc = element.Desc;
            }
            var node = cy.add({ group: 'nodes', data: { id: name, type: element.Key } });
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
        cy.on("render cyCanvas.resize", function (evt) {
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
                ctx.fillStyle = "black";
                ctx.font = "16px sans-serif";
                ctx.textAlign = "center";
                ctx.textBaseline = "middle";
                ctx.fillText(eventChars, pos.x, pos.y);
            });
            ctx.restore();
        });
        var defaults = {
            container: false,
            viewLiveFramerate: 0,
            thumbnailEventFramerate: 30,
            thumbnailLiveFramerate: false,
            dblClickDelay: 200,
            removeCustomContainer: true,
            rerenderDelay: 100
        };
        cy.navigator(defaults);
    }
    function go(file) {
        $.ajax({
            url: "GetData",
            data: { "file": file }
        })
            .done(function (data) {
            main(JSON.parse(data.item1), JSON.parse(data.item2), JSON.parse(data.item3));
        });
    }
    exports_1("go", go);
    var dagre_1, cytoscape_1, cytoscape_qtip_1, cytoscape_dagre_1, cytoscape_navigator_1, cytoscape_canvas_1;
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
            }
        ],
        execute: function () {
        }
    };
});
