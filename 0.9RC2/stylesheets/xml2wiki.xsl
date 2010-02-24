<?xml version="1.0" encoding="UTF-8"?>
<!--
This file is part of the xframe software package
hosted at http://xframe.sourceforge.net

Copyright (c) 2003 Kurt Riede.
    
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:doc="http://xframe.sf.net/xsddoc/doc" exclude-result-prefixes="xs doc" version="1.0">
  <!--
    This transformation generates html from a XML schema documentation.
  -->
  <xsl:output indent="no" version="4.0" method="html" encoding="ISO-8859-1" omit-xml-declaration="yes" doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN" doctype-system="http://www.w3.org/TR/html4/loose.dtd"/>
  <!--
    Include templates to document XML
  -->
  <xsl:include href="xmldoc.xsl"/>
  <!--
    Include xsddoc utilities templates.
  -->
  <xsl:include href="util.xsl"/>
  <!--
    title parameter.
  -->
  <xsl:param name="doctitle"/>
  <!--
    header parameter.
  -->
  <xsl:param name="header"/>
  <!--
    footer parameter.
  -->
  <xsl:param name="footer"/>
  <!--
    bottom parameter.
  -->
  <xsl:param name="bottom"/>
  <!--
    Whether to show types in overview pages or not.
  -->
  <xsl:param name="hideTypes" select="string('false')"/>
  <!--
    Whether to show groups in overview pages or not.
  -->
  <xsl:param name="hideGroups" select="string('false')"/>
  <!--
    Whether to show attributes in overview pages or not.
  -->
  <xsl:param name="hideAttributes" select="string('false')"/>
  <!--
    Root template.
  -->
  <xsl:template match="/">
    <xsl:apply-templates select="*"/>
  </xsl:template>

  <xsl:output method="text"/>
	
  <xsl:template match="doc:xsddoc">#summary Golf Markup Language Documentation: <xsl:value-of select="doc:component/@type"/><xsl:text> </xsl:text><xsl:value-of select="doc:component/@name"/>.

== Name ==

<xsl:call-template name="GenWikiLink">
	<xsl:with-param name="name"><xsl:value-of select="doc:component/@name"/></xsl:with-param>
	<xsl:with-param name="type"><xsl:value-of select="doc:component/@type"/></xsl:with-param>
</xsl:call-template>

  	<xsl:apply-templates select="doc:component/doc:superTypes"/>
  	<xsl:text>
----
  	</xsl:text>
    <xsl:if test="doc:component/doc:documentation">
      <xsl:for-each select="doc:component">
        <xsl:call-template name="documentation"/>
      </xsl:for-each>
      <xsl:text>
----
      </xsl:text>
    </xsl:if>
    <xsl:if test="doc:component/doc:error">
      <xsl:text>An error occured:</xsl:text>
{{{
        <xsl:apply-templates select="doc:component/doc:error/text()"/>
}}}
----
    </xsl:if>
  	
===  Properties ===

  	<xsl:choose>
      <xsl:when test="@nillable = 'true'">
        <xsl:text>This component is nillable.</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>This component is not nillable.</xsl:text>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:if test="@abstract = 'true'">
      <xsl:text>This component is abstract.</xsl:text>

    </xsl:if>
    <xsl:if test="string(@final) != ''">
      <xsl:text>This component is final by: </xsl:text>
      <xsl:value-of select="@final"/>
      <xsl:text>.</xsl:text>

    </xsl:if>
    <xsl:if test="string(@block) != ''">
      <xsl:text>This component is blocked from derivation by: </xsl:text>
      <xsl:value-of select="@block"/>
      <xsl:text>.</xsl:text>

    </xsl:if>
    <xsl:if test="@mixed = 'true'">
      <xsl:text>This component allowes mixed content.</xsl:text>

    </xsl:if>
    <xsl:if test="@mixed = 'false'">
      <xsl:text>This component doesn't allow mixed content.</xsl:text>

    </xsl:if>
    <xsl:if test="@substitutionGroup">
      <xsl:text>This element is a substitution group for type </xsl:text>

        <xsl:choose>
          <xsl:when test="@substitutionGroupHref">
          	<xsl:call-template name="MakeAnchor">
          		<xsl:with-param name="url" select="@substitutionGroupHref"/>
          		<xsl:with-param name="name" select="@substitutionGroup"/>
          	</xsl:call-template>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="@substitutionGroup"/>
          </xsl:otherwise>
        </xsl:choose>

      <xsl:text>.</xsl:text>
      
    </xsl:if>
  	
  	<xsl:text>
----
  	</xsl:text>
  	
    <xsl:apply-templates select="doc:component/doc:model"/>
    <xsl:apply-templates select="doc:component/doc:subTypes"/>
    <xsl:apply-templates select="doc:component/doc:implementors"/>
    <xsl:apply-templates select="doc:component/doc:typeReference"/>
  </xsl:template>
  <!--
    Format super types.
  -->
  <xsl:template match="doc:superTypes">
    <xsl:if test="doc:type">
===  Super Types ===
{{{
<xsl:call-template name="superType">
	<xsl:with-param name="types" select="*"/>
</xsl:call-template>
}}}
    </xsl:if>
  </xsl:template>
  <!--
    Recursive format super types.
  -->
  <xsl:template name="superType">
    <xsl:param name="types"/>
    <xsl:param name="indent" select="string('')"/>
    <xsl:for-each select="$types[position() = 1]">
      <xsl:if test="$indent">
<xsl:value-of select="$indent"/><xsl:text>+--</xsl:text>
      </xsl:if>
      <xsl:choose>
        <xsl:when test="count($types) &gt; 1">
          <xsl:choose>
            <xsl:when test="@href"><xsl:call-template name="MakeAnchor">
            		<xsl:with-param name="url" select="@href"/>
            		<xsl:with-param name="name" select="@name"/>
            	</xsl:call-template></xsl:when>
            <xsl:otherwise><xsl:value-of select="@name"/></xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <xsl:otherwise><xsl:value-of select="@name"/></xsl:otherwise>
      </xsl:choose>
      <xsl:if test="@derivation"><xsl:text> (</xsl:text><xsl:value-of select="@derivation"/><xsl:text>)</xsl:text></xsl:if>
    	
    </xsl:for-each>
    <xsl:if test="count($types) &gt; 1">
      <xsl:variable name="newIndent">
        <xsl:choose>
          <xsl:when test="$indent"><xsl:value-of select="concat($indent, '     ')"/></xsl:when>
          <xsl:otherwise><xsl:value-of select="concat($indent, '  ')"/></xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:if test="$newIndent"><xsl:text>
</xsl:text><xsl:value-of select="$newIndent"/><xsl:text>|
</xsl:text>
      </xsl:if>
      <xsl:call-template name="superType">
        <xsl:with-param name="types" select="$types[position() &gt; 1]"/>
        <xsl:with-param name="indent" select="$newIndent"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>
  <!--
    Format sub types.
  -->
  <xsl:template match="doc:subTypes">
    <xsl:if test="doc:type/*">
===  Sub Types ===
{{{
<xsl:apply-templates select="doc:type" mode="subType"/>
}}}
----
    </xsl:if>
  </xsl:template>
  <!--
    Format sub types.
    todo: sort
  -->
  <xsl:template match="doc:type" mode="subType">
    <xsl:param name="indentBlk" select="string('')"/>
    <xsl:param name="indentTyp" select="string('')"/>
    <xsl:param name="indentSep" select="string('')"/>
    <xsl:if test="$indentBlk"><xsl:value-of select="$indentBlk"/><xsl:text>
</xsl:text></xsl:if><xsl:value-of select="$indentTyp"/><xsl:choose>
      <xsl:when test="@href">
      	<xsl:call-template name="MakeAnchor">
      		<xsl:with-param name="url" select="@href"/>
      		<xsl:with-param name="name" select="@name"></xsl:with-param>
      	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise><xsl:value-of select="@name"/></xsl:otherwise>
    </xsl:choose>
    <xsl:if test="@derivation">
      <xsl:text> (</xsl:text>
      <xsl:value-of select="@derivation"/>
      <xsl:text>)
</xsl:text>
    </xsl:if>
  	
    <xsl:if test="doc:type">
      <xsl:for-each select="doc:type">
        <xsl:sort select="@name"/>
        <xsl:apply-templates select="." mode="subType">
          <xsl:with-param name="indentBlk" select="concat($indentSep, '  |  ')"/>
          <xsl:with-param name="indentTyp">
            <xsl:choose>
              <xsl:when test="count(../*) &gt; position()">
                <xsl:value-of select="concat($indentSep, '  +--')"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="concat($indentSep, '  +--')"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:with-param>
          <xsl:with-param name="indentSep">
            <xsl:choose>
              <xsl:when test="count(../*) &gt; position()">
                <xsl:value-of select="concat($indentSep, '  |  ')"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="concat($indentSep, '     ')"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:with-param>
        </xsl:apply-templates>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>
  <!--
    Format implementors reference.
    todo: sort
  -->
  <xsl:template match="doc:implementors">
    <xsl:if test="doc:type">
===  Implementors ===
          <xsl:apply-templates select="doc:type" mode="typeList"/>
----
    </xsl:if>
  </xsl:template>
  <!--
    Format implementors reference.
    todo: sort
  -->
  <xsl:template match="doc:typeReference">
    <xsl:if test="doc:type">
===  Local Usage ===
    	<xsl:apply-templates select="doc:type" mode="typeList">
            <xsl:sort select="@name"/>
          </xsl:apply-templates>
    </xsl:if>
  </xsl:template>
  <!--
    Format list of types.
  -->
  <xsl:template match="doc:type" mode="typeList">
    <xsl:if test="position() &gt; 1">
      <xsl:text>, </xsl:text>
    </xsl:if>
  	<xsl:call-template name="MakeAnchor">
  		<xsl:with-param name="url" select="@href"/>
  		<xsl:with-param name="name" select="@name"></xsl:with-param>
  	</xsl:call-template>
  </xsl:template>
  <!--
    Format documentation.
  -->
  <xsl:template name="documentation">
===  Documentation ===

        <xsl:for-each select="doc:documentation">
          <xsl:choose>
            <xsl:when test="*">
              <xsl:apply-templates select="*|text()" mode="documentation"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates select="*|text()" mode="documentation"/>

            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>

    <xsl:if test="@source">
===  See also ===

    	<xsl:call-template name="MakeAnchor">
    		<xsl:with-param name="url" select="xs:annotation/xs:documentation/@source"/>
    		<xsl:with-param name="name" select="xs:annotation/xs:documentation/@source"/>
    	</xsl:call-template>
    </xsl:if>
  </xsl:template>

	<!--
    Format schema source.
  -->
  <xsl:template match="xs:*" mode="doc">
    <xsl:apply-templates select="." mode="xmldoc">
      <xsl:with-param name="children" select="*[local-name() != 'annotation']"/>
    </xsl:apply-templates>
  </xsl:template>
  <!--
    Format content model.
  -->
  <xsl:template match="doc:model">
===  Model ===

	<xsl:apply-templates select="." mode="model"/>

  	<xsl:variable name="nested" select="//doc:element"/>
  	<xsl:if test="doc:attribute | $nested">
  		<xsl:text>
----
  		</xsl:text>
  	</xsl:if>
  	<xsl:if test="doc:attribute">      
=== Attributes ===

|| Name || Description || Type || Use || Default || Fixed || Form ||
<xsl:apply-templates select="doc:attribute[not(@any)]" mode="summary">
    <xsl:sort select="@name"/>
</xsl:apply-templates>   	

  	</xsl:if>
  	<xsl:if test="$nested">
=== Nested Elements ===

|| Name || Description || Type ||
<xsl:apply-templates select="$nested" mode="summary">
  <xsl:sort select="@name"/>
</xsl:apply-templates>
    	
  	</xsl:if>
  	<xsl:if test="doc:attribute | $nested">
  		<xsl:text>
----
  		</xsl:text>
  	</xsl:if>
  </xsl:template>
  <!--
    Format the attribute model.
  -->
  <xsl:template match="doc:attribute" mode="detail">
  	<!-- All detail elements sent to compact, table format in attribute summary. -->
  </xsl:template>
  <!--
    Format the attribute summary.
  -->
  <xsl:template match="doc:attribute" mode="summary">|| <xsl:value-of select="@name"
	/> ||  <xsl:apply-templates select="doc:*" mode="short-documentation"
  	/> || <xsl:choose><xsl:when test="@base">_<xsl:value-of select="@base"/>_</xsl:when>
              <xsl:when test="@href"><xsl:call-template name="MakeAnchor">
              		<xsl:with-param name="url" select="@href"/>
              		<xsl:with-param name="name" select="@type"/>
              </xsl:call-template></xsl:when><xsl:otherwise><xsl:value-of select="@type"/></xsl:otherwise></xsl:choose
  	> || <xsl:value-of select="@use"/> || <xsl:value-of select="@default"/> || <xsl:value-of select="@fixed"/> || <xsl:value-of select="@form"/> ||
</xsl:template>
  <!--
    Format the nested element summary.
  -->
  <xsl:template match="doc:element" mode="summary">
    <xsl:variable name="prefix" select="substring-before(@type, ':')"/>
    <xsl:variable name="ns" select="namespace::*[name() = $prefix]"/>|| <xsl:call-template name="MakeAnchor">
		<xsl:with-param name="url" select="@href"/>
		<xsl:with-param name="name" select="@name"/></xsl:call-template
  	> ||  <xsl:apply-templates select="doc:*[position() = 1]" mode="short-documentation"/> || <xsl:choose>
  		<xsl:when test="@type and ($ns = 'http://www.w3.org/2001/XMLSchema')"
  		><xsl:value-of select="@type"/></xsl:when
  		><xsl:when test="@type and @href"><xsl:call-template name="MakeAnchor">
  			<xsl:with-param name="url" select="@href"/>
  			<xsl:with-param name="name" select="@type"/>
  		</xsl:call-template></xsl:when
  		><xsl:when test="@type"><xsl:value-of select="@type"/></xsl:when
  		></xsl:choose> ||
</xsl:template>
  <!--
    Format the nested element model.
  -->
  <xsl:template match="doc:element" mode="detail">
  	<!-- All detail elements sent to compact, table format in nested element summary. -->
  </xsl:template>
  <!--
    Format the model.
  -->
  <xsl:template match="doc:model" mode="model">
    <xsl:choose>
      <xsl:when test="../@type = 'complexType'"><xsl:text>&lt;...</xsl:text></xsl:when>
      <xsl:when test="../@type = 'element'">
        <xsl:text>&lt;</xsl:text>
      	<xsl:call-template name="GenWikiLink">
      		<xsl:with-param name="name"><xsl:value-of select="../@name"/></xsl:with-param>
      		<xsl:with-param name="type"><xsl:text>element</xsl:text></xsl:with-param>
      	</xsl:call-template>
      </xsl:when>
    </xsl:choose>
    <xsl:apply-templates select="doc:attribute[@name] | doc:simpleContent/doc:*/doc:attribute | doc:simpleContent/doc:*/doc:attributeGroup" mode="particle">
      <xsl:with-param name="indent" select="string('    ')"/>
      <xsl:sort select="@name"/>
    </xsl:apply-templates>
    <xsl:apply-templates select="doc:attribute[@any]" mode="particle">
      <xsl:with-param name="indent" select="string('    ')"/>
      <xsl:sort select="@any"/>
    </xsl:apply-templates>
    <xsl:if test="../@type = 'complexType' or ../@type = 'element'">
      <xsl:text>>
      </xsl:text><xsl:value-of select="string('    ')"/>
    </xsl:if>
    
    <xsl:apply-templates select="doc:sequence | doc:element | doc:choice | doc:all | doc:restriction | doc:list | doc:union | doc:type" mode="particle">
      <xsl:with-param name="indent" select="string('  ')"/>
    </xsl:apply-templates>
    
    <xsl:choose>
      <xsl:when test="../@type = 'complexType'"><xsl:text>
</xsl:text><xsl:value-of select="concat('  ','&lt;/...&gt;')"/>
      </xsl:when>
      <xsl:when test="../@type = 'element'">
      	<xsl:call-template name="GenWikiLink">
      		<xsl:with-param name="name"><xsl:value-of select="../@name"/></xsl:with-param>
      		<xsl:with-param name="type"><xsl:text>element</xsl:text></xsl:with-param>
      		<xsl:with-param name="display_name"><xsl:value-of select="concat('&lt;/', ../@name, '&gt;')"/></xsl:with-param>
      	</xsl:call-template>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <!--
    Format model of local elements.
  -->
  <xsl:template match="doc:element" mode="model">
    <xsl:text>&lt;</xsl:text>
  	<xsl:call-template name="GenWikiLink">
  		<xsl:with-param name="name"><xsl:value-of select="@name"/></xsl:with-param>
  		<xsl:with-param name="type">element</xsl:with-param>
  	</xsl:call-template>   
  	<xsl:apply-templates select="doc:attribute[@name] | doc:simpleContent/doc:*/doc:attribute | doc:simpleContent/doc:*/doc:attributeGroup" mode="particle">
      <xsl:with-param name="indent" select="string('  ')"/>
      <xsl:sort select="@name"/>
    </xsl:apply-templates>
    <xsl:apply-templates select="doc:attribute[@any]" mode="particle">
      <xsl:with-param name="indent" select="string('  ')"/>
      <xsl:sort select="@any"/>
    </xsl:apply-templates>
    <xsl:text>>
</xsl:text>
    
    <xsl:apply-templates select="doc:sequence | doc:element | doc:choice | doc:all | doc:restriction | doc:list | doc:union | doc:type" mode="particle">
      <xsl:with-param name="indent" select="string('  ')"/>
    </xsl:apply-templates>
    
  	<xsl:call-template name="GenWikiLink">
  		<xsl:with-param name="name"><xsl:value-of select="@name"/></xsl:with-param>
  		<xsl:with-param name="type"><xsl:value-of select="@type"/></xsl:with-param>
  		<xsl:with-param name="display_name"><xsl:value-of select="concat('&lt;/', @name, '&gt;')"/></xsl:with-param>
  	</xsl:call-template>   
    
  </xsl:template>
  <!--
    Format attributes
  -->
  <xsl:template match="doc:attribute" mode="particle">
    
    <xsl:text disable-output-escaping="yes">  </xsl:text>
    <xsl:choose>
      <xsl:when test="@type">
        <xsl:choose>
          <xsl:when test="@use = 'required'">
            <xsl:apply-templates select="." mode="model-doc"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates select="." mode="model-doc"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="doc:simpleType">
        <xsl:choose>
          <xsl:when test="@use = 'required'">
            <xsl:apply-templates select="." mode="model-doc"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates select="." mode="model-doc"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        _<xsl:text>{</xsl:text><xsl:value-of select="@any"/><xsl:text>}</xsl:text>_
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!--
    Format attribute model.
  -->
  <xsl:template match="doc:attribute" mode="model-doc">
  	<xsl:text>`</xsl:text>
    <xsl:value-of select="@name"/>
  	<xsl:text>`</xsl:text>
  	<xsl:text>=</xsl:text>
    <xsl:variable name="typeName">
      <xsl:choose>
        <xsl:when test="string(@type) != ''">
          <xsl:value-of select="@type"/>
        </xsl:when>
        <xsl:when test="string(@base) != ''">
          <xsl:value-of select="@base"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="doc:simpleType" mode="particle"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="@base">
        _<xsl:value-of select="@base"/>_
      </xsl:when>
      <xsl:when test="@href">
      	<xsl:call-template name="MakeAnchor">
      		<xsl:with-param name="url" select="@href"/>
      		<xsl:with-param name="name" select="@typeName"/>
      	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$typeName"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="string(@default) != ''">
      <xsl:text> : </xsl:text>
      <xsl:value-of select="@default"/>
    </xsl:if>
  </xsl:template>
  <!--
    Ignore documentation inside of compositors.
  -->
  <xsl:template match="doc:documentation" mode="particle">
  </xsl:template>
  <!--
    Format simpleType model.
  -->
  <xsl:template match="doc:simpleType" mode="model-doc">
    <xsl:apply-templates select="doc:*" mode="model"/>
  </xsl:template>
  <!--
    Format sequence or group model.
  -->
  <xsl:template match="doc:sequence | doc:group" mode="particle">
    <xsl:param name="indent" select="string('')"/>
    <xsl:variable name="count" select="count(*[local-name() != 'documentation'])"/>
    <xsl:value-of select="$indent"/>
    <xsl:if test="$count &gt; 1">
      <xsl:text>( </xsl:text>
    </xsl:if>
    <xsl:for-each select="*">
      <xsl:apply-templates select="." mode="particle"/>
      <xsl:if test="position() &lt; $count">
        <xsl:text>, </xsl:text>
      </xsl:if>
    </xsl:for-each>
    <xsl:value-of select="$indent"/>
    <xsl:if test="$count &gt; 1">
      <xsl:text> )</xsl:text>
    </xsl:if>
    <xsl:call-template name="FormatOccurs">
    	<xsl:with-param name="occurs"><xsl:value-of select="@occurs"/></xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <!--
    Format choice model.
  -->
  <xsl:template match="doc:choice" mode="particle">
    <xsl:param name="indent" select="string('')"/>
    <xsl:variable name="count" select="count(*[local-name() != 'documentation'])"/>
    <xsl:value-of select="$indent"/>
    <xsl:if test="$count &gt; 1">
      <xsl:text>( </xsl:text>
    </xsl:if>
    <xsl:for-each select="*">
      <xsl:apply-templates select="." mode="particle"/>
      <xsl:if test="position() &lt; $count">
        <xsl:text> | </xsl:text>
      </xsl:if>
    </xsl:for-each>
    <xsl:if test="$count &gt; 1">
      <xsl:text> )</xsl:text>
    </xsl:if>
        <xsl:call-template name="FormatOccurs">
    	<xsl:with-param name="occurs"><xsl:value-of select="@occurs"/></xsl:with-param>
    </xsl:call-template>
    <xsl:text> </xsl:text>
  </xsl:template>
  <!--
    Format all model.
  -->
  <xsl:template match="doc:all" mode="particle">
    <xsl:param name="indent" select="string('')"/>
    <xsl:variable name="count" select="count(*[local-name() != 'documentation'])"/>
    <xsl:value-of select="$indent"/>
    <xsl:if test="$count &gt; 1">
      <xsl:text>( </xsl:text>
    </xsl:if>
    <xsl:for-each select="*">
      <xsl:apply-templates select="." mode="particle"/>
      <xsl:if test="position() &lt; $count">
        <xsl:text> ; </xsl:text>
      </xsl:if>
    </xsl:for-each>
    <xsl:if test="$count &gt; 1">
      <xsl:text> )</xsl:text>
    </xsl:if>
        <xsl:call-template name="FormatOccurs">
    	<xsl:with-param name="occurs"><xsl:value-of select="@occurs"/></xsl:with-param>
    </xsl:call-template>
    <xsl:text> </xsl:text>
  </xsl:template>
  <!--
    Format any model.
  -->
  <xsl:template match="doc:any" mode="particle">
    <xsl:param name="indent" select="string('')"/>
    <xsl:value-of select="$indent"/>
    <xsl:text>( </xsl:text>
    _<xsl:text>any element from </xsl:text>
      <xsl:choose>
        <!-- when the namespace is specified and the value is not ##any -->
        <xsl:when test="@namespace and not(@namespace = '##any') and not(@namespace = '')">
          <xsl:choose>
            <xsl:when test="@namespace = '##other'">
              <xsl:text>any other namespace</xsl:text>
            </xsl:when>
            <xsl:when test="@namespace = '##targetNamespace' or @namespace='##local'">
              <xsl:text>local namespace </xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>namespace </xsl:text>
              <xsl:value-of select="@namespace"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <!-- Either the namespace is not specified or its value is ##any -->
        <xsl:otherwise>
          <xsl:text>any namespace</xsl:text>
        </xsl:otherwise>
      </xsl:choose>_
    <xsl:text> )</xsl:text>
        <xsl:call-template name="FormatOccurs">
    	<xsl:with-param name="occurs"><xsl:value-of select="@occurs"/></xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <!--
    Format element particles.
  -->
  <xsl:template match="doc:element | doc:type" mode="particle">
    <xsl:param name="separator" select="string(',')"/>
    <xsl:param name="indent" select="string('')"/>
    <xsl:param name="count"/>
  	<xsl:call-template name="MakeAnchor">
  		<xsl:with-param name="url"><xsl:value-of select="@href"/></xsl:with-param>
  		<xsl:with-param name="name"><xsl:value-of select="@name"/></xsl:with-param>
  	</xsl:call-template>
    <xsl:call-template name="FormatOccurs">
    	<xsl:with-param name="occurs"><xsl:value-of select="@occurs"/></xsl:with-param>
    </xsl:call-template>
  </xsl:template>
			
  <!--
    Format complexType particles.
  -->
  <xsl:template match="doc:complexType" mode="particle">
    <xsl:text>...</xsl:text>
  </xsl:template>
  <!--
    simple type restriction documentation.
  -->
  <xsl:template match="doc:restriction" mode="particle">
    <xsl:if test="doc:enumeration">
      <xsl:choose>
        <xsl:when test="count(doc:enumeration) = 1">
          <xsl:text>'</xsl:text>
          <xsl:value-of select="doc:enumeration[position() = 1]/@value"/>
          <xsl:text>'</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>( </xsl:text>
          <xsl:text>'</xsl:text>
          <xsl:value-of select="doc:enumeration[position() = 1]/@value"/>
          <xsl:text>'</xsl:text>
          <xsl:if test="doc:enumeration[position() > 1]">
            <xsl:for-each select="doc:enumeration[position() > 1]">
              <xsl:text> | '</xsl:text>
              <xsl:value-of select="@value"/>
              <xsl:text>'</xsl:text>
            </xsl:for-each>
          </xsl:if>
          <xsl:text> )</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:if test="doc:pattern">
{{{
        <xsl:value-of select="doc:pattern/@value"/>
}}}
      
    </xsl:if>
    <xsl:if test="doc:totalDigits">
      <xsl:text>maximum number of digits: </xsl:text>
      <xsl:value-of select="doc:totalDigits/@value"/>
      
    </xsl:if>
    <xsl:if test="doc:length">
      <xsl:text>length: </xsl:text>
      <xsl:value-of select="doc:length/@value"/>
      
    </xsl:if>
    <xsl:if test="doc:minLength">
      <xsl:text>minimum length: </xsl:text>
      <xsl:value-of select="doc:minLength/@value"/>
      
    </xsl:if>
    <xsl:if test="doc:maxLength">
      <xsl:text>maximum length: </xsl:text>
      <xsl:value-of select="doc:maxLength/@value"/>
      
    </xsl:if>
    <xsl:if test="doc:whiteSpace">
      <xsl:text>attribute-value normalization: </xsl:text>
      <xsl:value-of select="doc:whiteSpace/@value"/>
      
    </xsl:if>
    <xsl:if test="doc:minExclusive">
      <xsl:text>exclusive lower bound: </xsl:text>
      <xsl:value-of select="doc:minExclusive/@value"/>
      
    </xsl:if>
    <xsl:if test="doc:maxExclusive">
      <xsl:text>exclusive upper bound: </xsl:text>
      <xsl:value-of select="doc:maxExclusive/@value"/>
      
    </xsl:if>
    <xsl:if test="doc:minInclusive">
      <xsl:text>inclusive lower bound: </xsl:text>
      <xsl:value-of select="doc:minInclusive/@value"/>
      
    </xsl:if>
    <xsl:if test="doc:maxInclusive">
      <xsl:text>inclusive upper bound: </xsl:text>
      <xsl:value-of select="doc:maxInclusive/@value"/>
      
    </xsl:if>
  </xsl:template>
  <!--
    simple type list documentation.
  -->
  <xsl:template match="doc:list" mode="particle">
    <xsl:text>List of </xsl:text>
    <xsl:choose>
      <xsl:when test="@itemType">
        <xsl:value-of select="@itemType"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="doc:simpleType/doc:*" mode="particle"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!--
    simple type union documentation.
  -->
  <xsl:template match="doc:union" mode="particle">
    <xsl:text>( </xsl:text>
    <xsl:if test="@memberTypes">
      <xsl:value-of select="@memberTypes"/>
      <xsl:if test="doc:simpleType">
        <xsl:text> | </xsl:text>
      </xsl:if>
    </xsl:if>
    <xsl:apply-templates select="doc:simpleType[position() = 1]/doc:*" mode="particle"/>
    <xsl:for-each select="doc:simpleType[position() > 1]">
      <xsl:text> | </xsl:text>
      <xsl:apply-templates select="doc:*" mode="particle"/>
    </xsl:for-each>
    <xsl:text> )</xsl:text>
  </xsl:template>
  <!--
    Format documentation.
    If there is xhtml or other unknown XML inside, pass-thru this XML for later formatting.
    Else output as preformatted text with a {{{pre}}} tag.
  -->
  <xsl:template match="doc:documentation" mode="documentation">
    <xsl:if test="doc:documentation/@source">
      <xsl:attribute name="href"><xsl:value-of select="doc:documentation/@source"/></xsl:attribute>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="*|text()">
        <xsl:apply-templates select="*|text()"/>
      </xsl:when>
      <xsl:otherwise>
          <xsl:apply-templates select="*|text()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!--
  -->
  <xsl:template match="*" mode="documentation">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()|text()" mode="documentation"/>
    </xsl:copy>
  </xsl:template>
  <!--
  -->
  <xsl:template match="@*" mode="documentation">
    <xsl:attribute name="{name()}"><xsl:value-of select="."/></xsl:attribute>
  </xsl:template>
  <!--
  -->
  <xsl:template match="text()" mode="documentation">
    <xsl:value-of select="."/>
  </xsl:template>
  <!--
    Format doc:source.
  -->
  <xsl:template match="doc:source">
  	<xsl:copy-of select="*"/>
  </xsl:template>
  <!--
    Format documentation as a short abstract (upto the first dot).
    If there is xhtml or other unknown XML inside, pass-thru this XML for later formatting.
    Else output as preformatted text with a {{{pre}}} tag.
  -->
  <xsl:template match="doc:documentation" mode="short-documentation">
    <xsl:call-template name="short-documentation">
      <xsl:with-param name="nodes" select="* | text()"/>
    </xsl:call-template>
  </xsl:template>
  <!--
  -->
  <xsl:template name="short-documentation">
    <xsl:param name="nodes"/>
    <xsl:apply-templates select="$nodes[position() = 1]" mode="abstract">
      <xsl:with-param name="rest" select="$nodes[position() &gt; 1]"/>
    </xsl:apply-templates>
  </xsl:template>
  <!--
    Find and return all text upto the first dot.
  -->
  <xsl:template match="text()" mode="abstract">
    <xsl:param name="rest"/>
    <xsl:choose><xsl:when test="contains(normalize-space(), '. ')"
    	><xsl:value-of select="translate(substring-before(normalize-space(), '. '), '&#xA;', '')"
    	/></xsl:when><xsl:otherwise><xsl:value-of select="translate(., '&#xA;', '')"
      /><xsl:call-template name="short-documentation">
          <xsl:with-param name="nodes" select="$rest"/>
        </xsl:call-template></xsl:otherwise></xsl:choose>
  </xsl:template>
  <!--
  -->
  <xsl:template match="*" mode="abstract">
    <xsl:param name="rest"/>
    <xsl:copy>
      <xsl:call-template name="short-documentation">
        <xsl:with-param name="nodes" select="* | text()"/>
      </xsl:call-template>
      <xsl:choose>
        <xsl:when test="contains(., '.')"></xsl:when>
        <xsl:otherwise><xsl:call-template name="short-documentation">
            <xsl:with-param name="nodes" select="$rest"/>
          </xsl:call-template></xsl:otherwise>
      </xsl:choose>
    </xsl:copy>
  </xsl:template>
  <!--
  -->
  <xsl:template match="@*" mode="doc-attr">
    <xsl:attribute name="{name(.)}"><xsl:value-of select="."/></xsl:attribute>
  </xsl:template>
  <!--
    Navigation bar placed on top and bottom of overview page.
  -->
  <xsl:template name="navigationOverview">
    <xsl:call-template name="navigationHandler">
      <xsl:with-param name="rootFolder" select="'./'"/>
      <xsl:with-param name="namespaceFolder" select="/xs:schema/@targetNamespace"/>
      <xsl:with-param name="selected" select="'overview'"/>
    </xsl:call-template>
  </xsl:template>
  <!--
    Navigation bar placed on top and bottom of namespace page.
  -->
  <xsl:template name="navigationNamespace">
    <xsl:call-template name="navigationHandler">
      <xsl:with-param name="rootFolder" select="'../'"/>
      <xsl:with-param name="namespaceFolder" select="'./'"/>
      <xsl:with-param name="selected" select="'namespace'"/>
    </xsl:call-template>
  </xsl:template>
  <!--
    Navigation bar placed on top and bottom of component pages.
  -->
  <xsl:template name="navigationComponent">
    <xsl:call-template name="navigationHandler">
      <xsl:with-param name="rootFolder" select="'../../'"/>
      <xsl:with-param name="namespaceFolder" select="'../'"/>
      <xsl:with-param name="selected" select="'component'"/>
    </xsl:call-template>
  </xsl:template>
  <!--
    Navigation bar placed on top and bottom of index-all pages.
  -->
  <xsl:template name="navigationIndex">
    <xsl:call-template name="navigationHandler">
      <xsl:with-param name="rootFolder" select="'./'"/>
      <xsl:with-param name="namespaceFolder" select="'./'"/>
      <xsl:with-param name="selected" select="'index'"/>
    </xsl:call-template>
  </xsl:template>
  <!--
    Navigation bar placed on top and bottom of component pages.
  -->
  <xsl:template name="navigationHelp">
    <xsl:call-template name="navigationHandler">
      <xsl:with-param name="rootFolder" select="'./'"/>
      <xsl:with-param name="namespaceFolder" select="'./'"/>
      <xsl:with-param name="selected" select="'help'"/>
    </xsl:call-template>
  </xsl:template>
  <!--
    Navigation bar placed on top and bottom of page.
  -->
  <xsl:template name="navigationHandler">
    <xsl:param name="rootFolder" select="'../../'"/>
    <xsl:param name="namespaceFolder" select="'../'"/>
    <xsl:param name="selected" select="'overview'"/>
  </xsl:template>
  <!--
    Default bottom.
  -->
  <xsl:template name="bottom">
  </xsl:template>

  <xsl:template match="doc:help">
  </xsl:template>
	
	<xsl:template name="FormatOccurs">
		<xsl:param name="occurs"/>
		<xsl:if test="$occurs = '*'"><xsl:text>`*`</xsl:text></xsl:if>
		<xsl:if test="$occurs != '*'"><xsl:value-of select="$occurs"/></xsl:if>
	</xsl:template>
	
	<xsl:template name="MakeAnchor">
		<xsl:param name="url"/>
		<xsl:param name="name"/>
		
		<xsl:param name="type">
			<!--  href="../../http___code.google.com_p_golfml/complexType/Media.html" -->
			<xsl:if test="string-length($url)=0"><xsl:text>?</xsl:text></xsl:if>
			<xsl:if test="$url != ''">
				<xsl:value-of select="substring-before(substring-after(substring-after(substring-after($url, '/'), '/'), '/'), '/')"/>
			</xsl:if>
		</xsl:param>
		<xsl:param name="start_type">
			<xsl:choose>
				<xsl:when test="$type = 'attributeGroup'">A</xsl:when>
				<xsl:when test="$type = 'group'">G</xsl:when>
				<xsl:when test="$type = 'element'">E</xsl:when>
				<xsl:when test="$type = 'simpleType'">S</xsl:when>
				<xsl:when test="$type = 'complexType'">C</xsl:when>
				<xsl:when test="$type = 'substitutionGroup'">S</xsl:when>
				<xsl:otherwise>X</xsl:otherwise>
			</xsl:choose>
		</xsl:param>
		
		<xsl:param name="wikilink">
			<xsl:if test="string-length($url)=0">
				<xsl:value-of select="$name"/>
			</xsl:if>
			<xsl:if test="$url != ''">
				<xsl:value-of select="substring-before(substring-after(substring-after(substring-after(substring-after($url, '/'), '/'), '/'), '/'), '.')"/><!-- remove .html -->
			</xsl:if>
		</xsl:param>
		
		<xsl:if test="$url != ''">
		<xsl:choose>
			<xsl:when test="$name"><xsl:value-of select="concat('[', $start_type, $wikilink, ' ', $name, ']')"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="concat('[', $start_type, $wikilink, ' ', $wikilink, ']')"/></xsl:otherwise>
		</xsl:choose>
		</xsl:if>
		<xsl:if test="string-length($url)=0"><xsl:value-of select="$name"/></xsl:if>

	</xsl:template>
	
	<xsl:template name="GenWikiLink">
		<xsl:param name="name"/>
		<xsl:param name="display_name"/>
		<xsl:param name="type"/>
		<xsl:param name="display">
			<xsl:if test="string-length($display_name)=0">
				<xsl:value-of select="$name"/>
			</xsl:if>
			<xsl:if test="string-length($display_name)>0">
				<xsl:value-of select="$display_name"/>
			</xsl:if>
		</xsl:param>
		<xsl:param name="start_type">
			<xsl:choose>
				<xsl:when test="$type = 'attributeGroup'">A</xsl:when>
				<xsl:when test="$type = 'group'">G</xsl:when>
				<xsl:when test="$type = 'element'">E</xsl:when>
				<xsl:when test="$type = 'simpleType'">S</xsl:when>
				<xsl:when test="$type = 'complexType'">C</xsl:when>
				<xsl:when test="$type = 'substitutionGroup'">S</xsl:when>
				<xsl:otherwise>X</xsl:otherwise>
			</xsl:choose>
		</xsl:param>
		<xsl:value-of select="concat('[', $start_type,$name,' ',$display, ']')"/>
	</xsl:template>

</xsl:stylesheet>