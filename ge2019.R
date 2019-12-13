# https://yougov.co.uk/topics/politics/articles-reports/2019/11/27/how-yougovs-2019-general-election-model-works
dec=read.xlsx("https://d25d2506sfb94s.cloudfront.net/cumulus_uploads/document/1dqzvzxzqc/YouGov%20MRP%20December%202019.xlsx")
nov=read.csv("https://yg-infographics-data.s3-eu-west-1.amazonaws.com/ZAfbtHgj42wx4reHnaMtbBamoKdMxkFMpz4gnWMjiZCUAxDX66MsCB38K/2019_data/party_constituency_vote_shares.csv")
nov=nov[,names(dec)]
names(nov)=paste0("nov.", names(nov))

names(dec)=paste0("dec.", names(dec))
mrp=merge(dec,nov,by.y=c("nov.code"),by.x=c("dec.code"))
mrp=mrp[,grep("nov.constit", names(mrp), invert = T)]

js=rjson::fromJSON(file="https://interactive.guim.co.uk/2019/12/ukelection2019-data/prod/snap/full.json")

results=data.frame()
for (i in 1:length(js)) {
  x=js[[i]]
 if ("candidates" %in% names(x)) {
   print(i)
   parties=data.frame(code=x$ons)
   for (j in 1:length(x$candidates)) {
     party=x$candidates[[j]]
     parties[,party$party]=party$percentageShare
   }
   parties=cbind(parties, as.data.frame(x)[,c("mtime","turnout","majority","electorate","percentageTurnout","percentageChangeTurnout","swing","lastPercentageMajority","sittingParty","winningParty")])
   results=rbind.fill(results, parties)
 }
}
m=merge(results, mrp, by.x="code", by.y="dec.code")

write.csv(x=m, file="combined.csv", row.names = F)
