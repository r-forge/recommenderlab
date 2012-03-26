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

public class recommender {

    public static String getRecommendations(String typeofrecommender, String files) throws FileNotFoundException, TasteException, IOException, OptionException {

                          if(typeofrecommender.equals("IRStatsRecommenderEvaluation")) { return irstatseval(files); }
			  else if(typeofrecommender.equals("SlopeOneRecommender")) { return rmsRec(files); }
			  else if(typeofrecommender.equals("knnRecommender")) { return knnRec(files); }
                          else if(typeofrecommender.equals("svdRecommender")) { return svdRec(files); }
			  else if(typeofrecommender.equals("userBasedRecommender")) {return userRec(files,6030, 10, 2); }

                  return "Invalid option";
    }
        public static String irstatseval(String files) throws FileNotFoundException, TasteException, IOException, OptionException {
        
	RandomUtils.useTestSeed();
          
         File ratingsFile = new File(files);                        
        DataModel model = new FileDataModel(ratingsFile);
        
	RecommenderIRStatsEvaluator evaluator = new GenericRecommenderIRStatsEvaluator();
     
        RecommenderBuilder builder = new RecommenderBuilder() {
			public Recommender buildRecommender(DataModel model) throws TasteException{
				UserSimilarity similarity = new PearsonCorrelationSimilarity(model);
				UserNeighborhood neighborhood = new NearestNUserNeighborhood(2, similarity, model);
				return new GenericUserBasedRecommender(model, neighborhood, similarity);
			}
		};

        
        IRStatistics stats = evaluator.evaluate(builder,
				null,
				model,
				null,
				2,
				GenericRecommenderIRStatsEvaluator.CHOOSE_THRESHOLD,
				1.0);
 
	String result = stats.getPrecision() + " " + stats.getRecall();
	System.out.println(stats.getPrecision());
	System.out.println(stats.getRecall());
	
	return result;
        } 

    public static String rmsRec(String files) throws FileNotFoundException, TasteException, IOException, OptionException {
               
        File ratingsFile = new File(files);                        
        DataModel model = new FileDataModel(ratingsFile);
        
        
        RecommenderBuilder builder = new RecommenderBuilder() {
			public Recommender buildRecommender(DataModel model) throws TasteException{
				return new CachingRecommender(new SlopeOneRecommender(model));
			}
		};

        RecommenderEvaluator evaluator = new RMSRecommenderEvaluator();
        double score = evaluator.evaluate(builder,
				null,
				model,
				0.9,
				1);
 
		System.out.println(score);
               return score +" ";  

        }        	

 public static String knnRec(String files) throws FileNotFoundException, TasteException, IOException, OptionException {
               
        File ratingsFile = new File(files);                        
        DataModel model = new FileDataModel(ratingsFile);

		final ItemSimilarity similarity = new LogLikelihoodSimilarity(model);
		final Optimizer optimizer = new NonNegativeQuadraticOptimizer();

        
        
        RecommenderBuilder builder = new RecommenderBuilder() {
			public Recommender buildRecommender(DataModel model) throws TasteException{
				return new CachingRecommender(new KnnItemBasedRecommender(model, similarity, optimizer, 2));
			}
		};

        RecommenderEvaluator evaluator = new RMSRecommenderEvaluator();
        double score = evaluator.evaluate(builder,
				null,
				model,
				0.9,
				1);
 
		System.out.println(score);
               return score+ "";  

        }        	

public static String svdRec(String files) throws FileNotFoundException, TasteException, IOException, OptionException {
        
        File ratingsFile = new File(files);                        
        DataModel model = new FileDataModel(ratingsFile);
        
        RecommenderBuilder builder = new RecommenderBuilder() {
			public Recommender buildRecommender(DataModel model) throws TasteException{
				//return cachingRecommender;
				return new CachingRecommender(new SVDRecommender(model,4000,10));
			}
		};

        RecommenderEvaluator evaluator = new RMSRecommenderEvaluator();
        double score = evaluator.evaluate(builder,
				null,
				model,
				0.9,
				1);
 
		//System.out.println(score);
                 return score+"";

        }        

public static String userRec(String files, int userid, int numofitems, int neighbor) throws FileNotFoundException, TasteException, IOException, OptionException {

	File ratingsFile = new File(files);                        
        DataModel model = new FileDataModel(ratingsFile);
        

//DataModel model = new FileDataModel(new File("datasets/movieRatings.dat")); 
UserSimilarity similarity = new PearsonCorrelationSimilarity(model);
UserNeighborhood neighborhood = new NearestNUserNeighborhood(neighbor, similarity, model);
Recommender recommender = new GenericUserBasedRecommender(model, neighborhood, similarity); 
List<RecommendedItem> recommendations = recommender.recommend(userid, numofitems); 
String recommendedlist = "";

for (RecommendedItem recommendation : recommendations) {
	System.out.println(recommendation);
	recommendedlist = recommendation +" ";
}

return recommendedlist;

}


}
