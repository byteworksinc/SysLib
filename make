unset exit

if {#} == 0
   for i in io i2 lm cd mm nl
      Newer obj/{i}.a {i}.asm
      if {Status} != 0
         set exit on
         echo assemble +e +t {i}.asm
         assemble +e +t {i}.asm
         unset exit
      end
   end
else
   set exit on
   for i
      assemble +e +t {i}.asm
   end
end

echo delete syslib
delete syslib

set list        io.a i2.a lm.a cd.a mm.a nl.a
for i in {list}
*  purge >3/tmp
   echo makelib syslib +obj/{i}
   makelib syslib +obj/{i}
end
