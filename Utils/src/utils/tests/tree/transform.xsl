<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

 <xsl:template match="*|@*">
   <xsl:if test="name()!='e'">
     <xsl:copy>
       <xsl:apply-templates select="@*"/>
       <xsl:apply-templates select="node()"/>
     </xsl:copy>
   </xsl:if>
</xsl:template>

</xsl:stylesheet>

