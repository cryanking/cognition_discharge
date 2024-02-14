FROM cryanking/cognitioncheck:1.1
## tag cryanking/cognitioncheck:1.2


RUN install2.r --error xgboost sandwich effectsize lmtest randtoolbox
RUN install2.r --error cutpointr
