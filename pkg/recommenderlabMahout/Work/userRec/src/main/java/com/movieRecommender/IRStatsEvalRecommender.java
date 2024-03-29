package com.movieRecommender;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;
import java.io.IOException;

import org.apache.commons.cli2.OptionException; 
import org.apache.mahout.cf.taste.common.TasteException;
import org.apache.mahout.cf.taste.impl.model.file.FileDataModel;
import org.apache.mahout.cf.taste.model.DataModel;
import org.apache.mahout.cf.taste.recommender.RecommendedItem;
import org.apache.mahout.cf.taste.impl.common.LongPrimitiveIterator;


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

public class IRStatsEvalRecommender {
    
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
}
