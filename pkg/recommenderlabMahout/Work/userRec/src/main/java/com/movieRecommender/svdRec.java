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
import org.apache.mahout.cf.taste.impl.recommender.svd.SVDRecommender;


import org.apache.mahout.cf.taste.eval.RecommenderBuilder;
import org.apache.mahout.cf.taste.eval.RecommenderEvaluator;
import org.apache.mahout.cf.taste.impl.eval.RMSRecommenderEvaluator;
import org.apache.mahout.cf.taste.recommender.Recommender;

public class svdRec {
    
    public static void main(String... args) throws FileNotFoundException, TasteException, IOException, OptionException {
        
        // create data source (model) - from the csv file            
        File ratingsFile = new File("datasets/movieRatings.dat");                        
        DataModel model = new FileDataModel(ratingsFile);
        
        // create a simple recommender on our data
        //final CachingRecommender cachingRecommender = new CachingRecommender(new SlopeOneRecommender(model));
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
 
		System.out.println(score);
                 

        }        
}
