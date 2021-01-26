export ROOT_WD=$HOME/src/analytical-solutions-trees/master
export AS_REDSHIFT_URI=postgresql://as-linear.db.csas.csa.comscore.com:5439/live
export AS_LINEAR_DMAP_IAM_ROLE=arn:aws:iam::822154877347:role/r-csas-as-linear-user,arn:aws:iam::354890984934:role/r-csps-csdmap-s3-user
export AS_LINEAR_IAM_ROLE=arn:aws:iam::822154877347:role/r-csas-as-linear-user,arn:aws:iam::809055356645:role/r-cssm-redshift-user

set -g fish_user_paths $fish_user_paths $ROOT_WD/bin
