# First, an easy calculation based on four cases and three variables

d.f<-data.frame(a=factor(c("a1","a1","a2","a2")),b=factor(c("b1","b2","b1","b2")),cc=factor(c("c1","c1","c1","c2")))
f<-factor(c("g1","g1","g2","g2"))
e.out<-list(a=table(f,d.f$a),b=table(f,d.f$b),cc=table(f,d.f$cc))
expect_that(tableByFactor(d.f,f,values="frequency"),is_equivalent_to(e.out))

e.out.2<-list(a=prop.table(table(f,d.f$a),1),b=prop.table(table(f,d.f$b),1),cc=prop.table(table(f,d.f$cc),1))
expect_that(tableByFactor(d.f,f,values="percentage"),is_equivalent_to(e.out.2))

# Now, with NA and 0 on data


d.f<-data.frame(a=factor(c(NA,"a1","a2","a2")),b=factor(c(0,0,1,1)),cc=factor(c("c1","c1","c1",NA)))
f<-factor(c("g1","g1","g2","g2"))
e.out<-list(a=table(f,d.f$a),b=table(f,d.f$b),cc=table(f,d.f$cc))
expect_that(tableByFactor(d.f,f,values="frequency"),is_equivalent_to(e.out))
e.out.2<-list(a=prop.table(table(f,d.f$a),1),b=prop.table(table(f,d.f$b),1),cc=prop.table(table(f,d.f$cc),1))
expect_that(tableByFactor(d.f,f,values="percentage"),is_equivalent_to(e.out.2))
