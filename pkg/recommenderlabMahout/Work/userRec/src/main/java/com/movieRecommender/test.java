package com.movieRecommender;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;
import java.io.IOException;

import org.apache.commons.cli2.OptionException; 
import org.apache.mahout.cf.taste.common.TasteException;
import org.apache.mahout.cf.taste.impl.model.file.FileDataModel;
import org.apache.mahout.cf.taste.impl.recommender.CachingRecommender;
import org.apache.mahout.cf.taste.impl.recommender.slopeone.SlopeOneRecommender;
import org.apache.mahout.cf.taste.model.DataModel;
import org.apache.mahout.cf.taste.recommender.RecommendedItem;
import org.apache.mahout.cf.taste.impl.common.LongPrimitiveIterator;


import org.apache.mahout.cf.taste.eval.RecommenderBuilder;
import org.apache.mahout.cf.taste.eval.RecommenderEvaluator;
import org.apache.mahout.cf.taste.impl.eval.RMSRecommenderEvaluator;
import org.apache.mahout.cf.taste.recommender.Recommender;

import org.apache.mahout.cf.taste.impl.model.file.*;
import org.apache.mahout.cf.taste.impl.neighborhood.*;
import org.apache.mahout.cf.taste.impl.recommender.*;
import org.apache.mahout.cf.taste.impl.similarity.*;
import org.apache.mahout.cf.taste.impl.model.file.FileDataModel;
import org.apache.mahout.cf.taste.common.*;
import org.apache.commons.cli2.OptionException; 
import org.apache.mahout.cf.taste.model.*;
import org.apache.mahout.cf.taste.neighborhood.*;
import org.apache.mahout.cf.taste.recommender.*;
import org.apache.mahout.cf.taste.similarity.*;

import org.apache.mahout.cf.taste.impl.recommender.knn.*;
import org.apache.mahout.cf.taste.impl.recommender.*;
import org.apache.mahout.cf.taste.impl.similarity.*;
import org.apache.mahout.cf.taste.impl.common.LongPrimitiveIterator;
import org.apache.mahout.cf.taste.impl.recommender.svd.SVDRecommender;
import org.apache.mahout.cf.taste.similarity.*;
import org.apache.mahout.cf.taste.model.DataModel;
import org.apache.mahout.cf.taste.recommender.RecommendedItem;

import org.apache.mahout.cf.taste.eval.RecommenderBuilder;
import org.apache.mahout.cf.taste.eval.RecommenderEvaluator;
import org.apache.mahout.cf.taste.impl.eval.RMSRecommenderEvaluator;
import org.apache.mahout.cf.taste.recommender.Recommender;

import org.apache.mahout.cf.taste.eval.RecommenderBuilder;
import org.apache.mahout.cf.taste.eval.RecommenderEvaluator;
import org.apache.mahout.cf.taste.eval.RecommenderIRStatsEvaluator;
import org.apache.mahout.cf.taste.impl.eval.IRStatisticsImpl;
import org.apache.mahout.cf.taste.eval.IRStatistics;
import org.apache.mahout.cf.taste.recommender.Recommender;
import org.apache.mahout.cf.taste.similarity.UserSimilarity;
import org.apache.mahout.cf.taste.neighborhood.UserNeighborhood;
import org.apache.mahout.cf.taste.impl.recommender.GenericUserBasedRecommender;
import org.apache.mahout.cf.taste.impl.similarity.PearsonCorrelationSimilarity;
import org.apache.mahout.cf.taste.impl.neighborhood.NearestNUserNeighborhood;
import org.apache.mahout.common.RandomUtils;
import org.apache.mahout.cf.taste.impl.eval.GenericRecommenderIRStatsEvaluator;

import org.apache.commons.cli2.OptionException; 
import org.apache.mahout.cf.taste.common.TasteException;
import org.apache.mahout.cf.taste.impl.model.file.FileDataModel;
import org.apache.mahout.cf.taste.impl.recommender.CachingRecommender;
import org.apache.mahout.cf.taste.impl.recommender.slopeone.SlopeOneRecommender;
import org.apache.mahout.cf.taste.model.DataModel;
import org.apache.mahout.cf.taste.recommender.RecommendedItem;
import org.apache.mahout.cf.taste.impl.common.LongPrimitiveIterator;

public class test {

    public static int[] createRecommender(String typeofrecommender, String files, int id) throws FileNotFoundException, TasteException, IOException, OptionException {

int[] unsuccessfulattempt = new int[1];

if(typeofrecommender.equals("userBasedRecommender")) {return getItems(recommend(createUBCF(files, 10, 2), id)); }
else if(typeofrecommender.equals("knnRecommender")) {return getItems(recommend(createKnnRecommender(files), id)); }
else if(typeofrecommender.equals("svdRecommender")) {return getItems(recommend(createSvdRecommender(files), id)); }
else if(typeofrecommender.equals("slopeOneRecommender")) {return getItems(recommend(createSlopeOneRecommender(files), id)); }
else if(typeofrecommender.equals("randomRecommender")) {return getItems(recommend(createRandomRecommender(files), id)); }
else if(typeofrecommender.equals("treeClusteringRecommender")) {return getItems(recommend(createTreeClusteringRecommender(files), id)); }
	
return unsuccessfulattempt;
 }

public static String getRecommendedList(List<RecommendedItem> recommendations){
String recommendedlist = "";
for (RecommendedItem recommendation : recommendations) {
	System.out.println(recommendation);
	recommendedlist = recommendation +" ";
}
return recommendedlist;
}

public static List<RecommendedItem> recommend(Recommender rec, int userid) throws TasteException {
List<RecommendedItem> recommendations = rec.recommend(userid, 10); 
return recommendations;
}

public static int[] getItems(List<RecommendedItem> list) {
int[] data = new int[list.size()];
for (int i = 0; i<list.size(); i++) {
	data[i]=(int)list.get(i).getItemID();
	System.out.println(list.get(i).getItemID());
}
return data;
}

public static Recommender createUBCF(String files, int numofitems, int neighbor) throws FileNotFoundException, TasteException, IOException, OptionException {

	File ratingsFile = new File(files);                        
        DataModel model = new FileDataModel(ratingsFile);
UserSimilarity similarity = new PearsonCorrelationSimilarity(model);
UserNeighborhood neighborhood = new NearestNUserNeighborhood(neighbor, similarity, model);

Recommender recommender = new GenericUserBasedRecommender(model, neighborhood, similarity); 
return recommender;
}

	public static Recommender createKnnRecommender(String files) throws FileNotFoundException, TasteException, IOException, OptionException {
               
        File ratingsFile = new File(files);                        
        DataModel model = new FileDataModel(ratingsFile);

		final ItemSimilarity similarity = new LogLikelihoodSimilarity(model);
		final Optimizer optimizer = new NonNegativeQuadraticOptimizer();

	return new KnnItemBasedRecommender(model, similarity, optimizer, 2);
	}        

public static Recommender createSvdRecommender(String files) throws FileNotFoundException, TasteException, IOException, OptionException {
        
        File ratingsFile = new File(files);                        
        DataModel model = new FileDataModel(ratingsFile);
        
        return new SVDRecommender(model,4000,10);

        }      

public static Recommender createSlopeOneRecommender(String files) throws FileNotFoundException, TasteException, IOException, OptionException {
               
        File ratingsFile = new File(files);                        
        DataModel model = new FileDataModel(ratingsFile);

	return new SlopeOneRecommender(model);
	}        

public static Recommender createRandomRecommender(String files) throws FileNotFoundException, TasteException, IOException, OptionException {
        
        File ratingsFile = new File(files);                        
        DataModel model = new FileDataModel(ratingsFile);
        
        return new RandomRecommender(model);

        }    

public static Recommender createTreeClusteringRecommender(String files) throws FileNotFoundException, TasteException, IOException, OptionException {
               
        File ratingsFile = new File(files);                        
        DataModel model = new FileDataModel(ratingsFile);

	UserSimilarity similarity = new LogLikelihoodSimilarity(model);
        ClusterSimilarity clusterSimilarity = new FarthestNeighborClusterSimilarity(similarity);

        return new TreeClusteringRecommender(model, clusterSimilarity, 10);

	}
}
