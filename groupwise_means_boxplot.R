genes1 <- row.names(ntc_ea_df[ntc_ea_df$EA5053213 == 2,])
genes2 <- row.names(ntc_ea_df[ntc_ea_df$EA5053214 == 2,])

#all_genes_intersect <- intersect(genes1,genes2)
all_genes_uniq <- unique(c(genes1,genes2))

#length(all_genes_intersect)
#length(all_genes_uniq)

tpm_bbrc_pc <- log2(tpm_bbrc_pc + 1)
tpm_ea_pc <- log2(tpm_ea_pc + 1)

bbrc <- as.data.frame(t(tpm_bbrc_pc))
ea <- as.data.frame(t(tpm_ea_pc))

bbrc <- bbrc[,colnames(bbrc) %in% all_genes_uniq]
ea <- ea[,colnames(ea) %in% all_genes_uniq]


type_bbrc <- meta_all$NASH_NHV[match(row.names(bbrc), meta_all$BBRC_ID)]
type_ea <- meta_all$NASH_NHV[match(row.names(ea), meta_all$EA_ID)]

bbrc <- add_column(bbrc, type_bbrc, .before = 1)
ea <- add_column(ea, type_ea, .before = 1)

bbrc <- bbrc[!bbrc$type %in% c("BMS_Ctrl","UHRR_Ctrl","HD200_Ctrl"),]
ea <- ea[!ea$type %in% c("BMS_Ctrl","UHRR_Ctrl","HD200_Ctrl"),]

# Taking column wise means by the sample type

means_bbrc <- bbrc%>%group_by(type_bbrc)%>%summarise_all("mean")
means_ea <- ea%>%group_by(type_ea)%>%summarise_all("mean")

melted_bbrc <- melt(means_bbrc)
melted_ea <- melt(means_ea)


bbrc_plot <- ggboxplot(melted_bbrc, x = "type_bbrc", y = "value",
          color = "type_bbrc", palette = good_colors, add="jitter") +
          theme_bw() +
          labs(x="", y = "Mean log2(TPM)\n", title="BBRC") +
          theme(plot.title=element_text(hjust=0.5)) +
          theme(legend.title=element_blank())



ea_plot <- ggboxplot(melted_ea, x = "type_ea", y = "value",
          color = "type_ea", palette = good_colors, add="jitter") +
          theme_bw() +
          labs(x="", y = "", title="EA") +
          theme(plot.title=element_text(hjust=0.5)) +
          theme(legend.title=element_blank())



plot <- ggarrange(bbrc_plot,ea_plot,ncol=2,nrow=1, legend="right", common.legend = T)
plot
ggsave(plot, filename = "NTC_EA_5k_genes.png",device="png", height=6, width=10)


