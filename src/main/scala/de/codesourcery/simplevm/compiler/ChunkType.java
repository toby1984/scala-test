package de.codesourcery.simplevm.compiler;

public enum ChunkType
{
    FILE_HEADER(0),
   CONSTANT_POOL(1),
   JUMP_TABLE(2),
   INSTRUCTIONS(3);

   public final int typeId;

   private ChunkType(int typeId) {
     this.typeId = typeId;
   }

   public int getTypeId() {
    return typeId;
   }

   public static ChunkType fromTypeId(int id) {
       for ( ChunkType t : values() ) {
           if ( t.typeId == id ) {
               return t;
           }
       }
       throw new IllegalArgumentException("Unknown chunk type ID "+id);
   }
}
