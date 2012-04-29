unit ugolfml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, eventlog, XMLRead, DOM;

implementation

procedure ReadSimpleXML;
(*
<?xml version="1.0"?>
<request>
  <request_type>PUT_FILE</request_type>
  <username>123</username>
  <password>abc</password>
</request>
*)
var
  PassNode: TDOMNode;
  Doc: TXMLDocument;
begin
  try
    // Read in xml file from disk
    ReadXMLFile(Doc, 'test.xml');
    // Retrieve the "password" node
    PassNode := Doc.DocumentElement.FindNode('password');
    // Write out value of the selected node
    WriteLn(PassNode.NodeValue); // will be blank
    // The text of the node is actually a separate child node
    WriteLn(PassNode.FirstChild.NodeValue); // correctly prints "abc"
    // alternatively
    WriteLn(PassNode.TextContent);
  finally
    // finally, free the document
    Doc.Free;
  end;
end;

procedure ReadComplexXML;
(*
<?xml version="1.0"?>
<images directory="mydir">
  <imageNode URL="graphic.jpg" title="">
    <Peca DestinoX="0" DestinoY="0">Pecacastelo.jpg1.swf</Peca>
    <Peca DestinoX="0" DestinoY="86">Pecacastelo.jpg2.swf</Peca>
  </imageNode>
</images>

OUTPUT:
imageNode graphic.jpg
Peca Pecacastelo.jpg1.swf (DestinoX=0; DestinoY=0)
Peca Pecacastelo.jpg2.swf (DestinoX=0; DestinoY=86)
*)
var
  Doc: TXMLDocument;
  Child: TDOMNode;
  j: integer;
begin
  try
    ReadXMLFile(Doc, 'test.xml');
    Memo.Lines.Clear;
    // using FirstChild and NextSibling properties
    Child := Doc.DocumentElement.FirstChild; //<imagenode.
    while Assigned(Child) do
    begin
      Memo.Lines.Add(Child.NodeName + ' ' + Child.Attributes.Item[0].NodeValue);
      // imageNode graphic.jpg
      // using ChildNodes method
      with Child.ChildNodes do
        try
          for j := 0 to (Count - 1) do // Loop though all the <peca>value</peca>
            Memo.Lines.Add(format('%s %s (%s=%s; %s=%s)',
              [Item[j].NodeName, //peca
              Item[j].FirstChild.NodeValue, // Pecacastelo.jpg1.swf
              Item[j].Attributes.Item[0].NodeName,
              // 1st attribute details
              Item[j].Attributes.Item[0].NodeValue,
              Item[j].Attributes.Item[1].NodeName,
              // 2nd attribute details
              Item[j].Attributes.Item[1].NodeValue]));
        finally
          Free;
        end;
      Child := Child.NextSibling;
    end;
  finally
    Doc.Free;
  end;
end;

end.

