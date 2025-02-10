
# Moda Dry
my_Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Moda_dry<-function(precip,caudal){
  pt <- monthly_precip[c("Fecha",precip)]# JTU01PT53 ATP01PT02
  prep <-pt %>% 
    mutate(year=year(Fecha)) %>%   
    mutate(mes=month(Fecha)) %>%   
    group_by(year) %>%  
    na.omit() %>%
    arrange(year, !!sym(precip)) %>% 
    slice_head(n=5) %>% # keep only the top 5 rows per year
    filter((mes - 1) %in% mes) %>%
    slice_head(n=2)
  
  mode <-inst_discharge[c("Date",caudal)] %>% #JTU01HQ32 ATP02HI01
    mutate_if(is.numeric,round,digits = 0) %>%
    mutate(year=year(Date)) %>% 
    mutate(mes=month(Date)) %>%   
    group_by(year,mes) %>% 
    na.omit %>%
    mutate(moda = my_Mode(!!sym(caudal))) %>% #JTU01HQ32
    #distinct(year, mes, moda) %>%
    distinct(year, mes, moda, .keep_all = FALSE) %>%
    merge(prep, by = c("year", "mes"))
  
  mode <- mode[!duplicated(mode$year), ]
  mode$moda <- mode$moda/1000

 
}
