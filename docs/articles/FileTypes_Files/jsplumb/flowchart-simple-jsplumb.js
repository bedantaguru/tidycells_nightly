jsPlumb.ready(function () {

    var instance = window.jsp = jsPlumb.getInstance({
        // default drag options
        DragOptions: { cursor: 'pointer', zIndex: 2000 },
        // the overlays to decorate each connection with.  note that the label overlay uses a function to generate the label text; in this
        // case it returns the 'labelText' member that we set on each connection in the 'init' method below.
        ConnectionOverlays: [
            [ "Arrow", {
                location: 1,
                visible:true,
                width:11,
                length:11,
                id:"ARROW",
                events:{
                    click:function() { alert("you clicked on the arrow overlay")}
                }
            } ],
            [ "Label", {
                location: 0.1,
                id: "label",
                cssClass: "aLabel",
                events:{
                    tap:function() { alert("hey"); }
                }
            }]
        ],
        Container: "canvas"
    });

    var basicType = {
        connector: "StateMachine",
        paintStyle: { stroke: "red", strokeWidth: 4 },
        hoverPaintStyle: { stroke: "blue" },
        overlays: [
            "Arrow"
        ]
    };
    instance.registerConnectionType("basic", basicType);

    // this is the paint style for the connecting lines..
    var connectorPaintStyle = {
            strokeWidth: 2,
            stroke: "#61B7CF",
            joinstyle: "round",
            outlineStroke: "white",
            outlineWidth: 2
        },
    // .. and this is the hover style.
        connectorHoverStyle = {
            strokeWidth: 3,
            stroke: "#216477",
            outlineWidth: 5,
            outlineStroke: "white"
        },
        endpointHoverStyle = {
            fill: "#216477",
            stroke: "#216477"
        },
    // the definition of source endpoints (the small blue ones)
        sourceEndpoint = {
            endpoint: "Dot",
            paintStyle: {
                fill: "transparent",
                radius: 0.1,
                strokeWidth: 1
            },
            isSource: true,
            connector: [ "Flowchart", { stub: [40, 60], gap: 10, cornerRadius: 5, alwaysRespectStubs: true } ],
            connectorStyle: connectorPaintStyle,
            hoverPaintStyle: endpointHoverStyle,
            connectorHoverStyle: connectorHoverStyle,
            dragOptions: {},
            overlays: [
                [ "Label", {
                    location: [0.5, 1.5],
                    label: "Drag",
                    cssClass: "endpointSourceLabel",
                    visible:false
                } ]
            ]
        },
    // the definition of target endpoints (will appear when the user drags a connection)
        targetEndpoint = {
            endpoint: "Dot",
            paintStyle: { fill: "transparent", radius: 0.1 },
            hoverPaintStyle: endpointHoverStyle,
            maxConnections: -1,
            dropOptions: { hoverClass: "hover", activeClass: "active" },
            isTarget: true,
            overlays: [
                [ "Label", { location: [0.5, -0.5], label: "Drop", cssClass: "endpointTargetLabel", visible:false } ]
            ]
        },
        init = function (connection) {
            //connection.getOverlay("label").setLabel(connection.sourceId.substring(15) + "-" + connection.targetId.substring(15));
        };

    var _addEndpoints = function (toId, sourceAnchors, targetAnchors) {
        for (var i = 0; i < sourceAnchors.length; i++) {
            var sourceUUID = toId + sourceAnchors[i];
            instance.addEndpoint("flowchart" + toId, sourceEndpoint, {
                anchor: sourceAnchors[i], uuid: sourceUUID
            });
        }
        for (var j = 0; j < targetAnchors.length; j++) {
            var targetUUID = toId + targetAnchors[j];
            instance.addEndpoint("flowchart" + toId, targetEndpoint, { anchor: targetAnchors[j], uuid: targetUUID });
        }
    };

    // suspend drawing and initialise.
    instance.batch(function () {

         _addEndpoints("Window2", ["RightMiddle", "BottomCenter", "LeftMiddle"], ["TopCenter"]);
_addEndpoints("Window3", ["BottomCenter"], ["LeftMiddle"]);
_addEndpoints("Window4", ["RightMiddle", "LeftMiddle"], ["TopCenter", "BottomCenter"]);
_addEndpoints("Window7", ["BottomCenter"], ["TopCenter"]);
_addEndpoints("Window8", ["BottomCenter", "LeftMiddle"], ["TopCenter"]);
_addEndpoints("Window9", ["LeftMiddle"], ["TopCenter"]);
_addEndpoints("Window10", ["RightMiddle", "LeftMiddle"], ["TopCenter"]);
_addEndpoints("Window11", ["BottomCenter", "LeftMiddle", "RightMiddle"], ["TopCenter"]);
_addEndpoints("Window12", ["RightMiddle"], ["TopCenter"]);
_addEndpoints("Window5", ["TopCenter", "BottomCenter", "LeftMiddle"], ["RightMiddle"]);
_addEndpoints("Window6", ["TopCenter", "BottomCenter", "RightMiddle"], ["LeftMiddle"]);

        // listen for new connections; initialise them the same way we initialise the connections at startup.
        instance.bind("connection", function (connInfo, originalEvent) {
            init(connInfo.connection);
        });

        // make all the window divs draggable
        instance.draggable(jsPlumb.getSelector(".flowchart-simple .window"), { grid: [20, 20] });

        // connect a few up
        instance.connect({uuids: ["Window2RightMiddle", "Window3LeftMiddle"]});
instance.connect({uuids: ["Window2BottomCenter", "Window4TopCenter"]});
instance.connect({uuids: ["Window2LeftMiddle", "Window5RightMiddle"]});
instance.connect({uuids: ["Window2RightMiddle", "Window6LeftMiddle"]});
instance.connect({uuids: ["Window3BottomCenter", "Window7TopCenter"]});
instance.connect({uuids: ["Window7BottomCenter", "Window8TopCenter"]});
instance.connect({uuids: ["Window8BottomCenter", "Window9TopCenter"]});
instance.connect({uuids: ["Window9LeftMiddle", "Window10TopCenter"]});
instance.connect({uuids: ["Window9LeftMiddle", "Window11TopCenter"]});
instance.connect({uuids: ["Window11BottomCenter", "Window12TopCenter"]});
instance.connect({uuids: ["Window12RightMiddle", "Window10TopCenter"]});
instance.connect({uuids: ["Window8LeftMiddle", "Window4BottomCenter"]});
instance.connect({uuids: ["Window10RightMiddle", "Window6LeftMiddle"]});
instance.connect({uuids: ["Window4RightMiddle", "Window6LeftMiddle"]});
instance.connect({uuids: ["Window10LeftMiddle", "Window5RightMiddle"]});
instance.connect({uuids: ["Window4LeftMiddle", "Window5RightMiddle"]});
instance.connect({uuids: ["Window11LeftMiddle", "Window5RightMiddle"]});
instance.connect({uuids: ["Window11RightMiddle", "Window6LeftMiddle"]});



    });

    jsPlumb.fire("jsPlumbLoaded", instance);

});
