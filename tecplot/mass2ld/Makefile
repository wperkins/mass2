SHELL=/bin/sh
LIB=$(ADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
STATLIB=$(ADDONLIBDIR)/lib$(LIBBASENAME).a
OBJDIR=obj/$(PLATFORM)/$(OBJSUBDIR)
IOUTILLIB=$(TEC75HOME)/lib/ext/$(PLATFORM)/libioutil.a
GUILIB=$(TEC75HOME)/lib/ext/$(PLATFORM)/libgui.a
NETCDFLIB=-lnetcdf 
shlib: $(LIB)
statlib: $(STATLIB)
#
# list of obj files....
#
OBJS=$(OBJDIR)/mass2ld.o $(OBJDIR)/guidefs.o $(OBJDIR)/guicb.o $(OBJDIR)/mass2file.o
 
#
# The next two dependencies probably can remain unchanged...
#
 
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o $(NETCDFLIB) $(GUILIB) -o $(LIB)
 
$(STATLIB) : $(OBJS)
	cd $(OBJDIR); $(AR) q lib$(LIBBASENAME).a *.o $(NETCDFLIB) $(GUILIB); mv lib$(LIBBASENAME).a  $(STATLIB)
 
#
# Duplicate the next line for additional source files...
#
 
$(OBJDIR)/mass2ld.o : mass2ld.c GUIDEFS.h mass2file.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../mass2ld.c 
$(OBJDIR)/guidefs.o : guidefs.c guibld.c GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../guidefs.c 
$(OBJDIR)/guicb.o : guicb.c guibld.c GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../guicb.c 
$(OBJDIR)/mass2file.o : mass2file.c mass2file.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../mass2file.c 
 
 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1
