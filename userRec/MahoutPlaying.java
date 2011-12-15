import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;
 
import org.apache.mahout.cf.taste.common.TasteException;
import org.apache.mahout.cf.taste.impl.model.file.FileDataModel;
import org.apache.mahout.cf.taste.impl.recommender.CachingRecommender;
import org.apache.mahout.cf.taste.impl.recommender.slopeone.SlopeOneRecommender;
import org.apache.mahout.cf.taste.model.DataModel;
import org.apache.mahout.cf.taste.recommender.RecommendedItem;
 
public class MahoutPlaying {
	public static void main(String[] args) throws FileNotFoundException, TasteException {
		DataModel model;
		model = new FileDataModel(new File("ratingsForMahout.dat"));
		CachingRecommender cachingRecommender = new CachingRecommender(new SlopeOneRecommender(model));
 
		List recommendations = cachingRecommender.recommend(1, 10);
		for (RecommendedItem recommendedItem : recommendations) {
			System.out.println(recommendedItem);
		}
 
	}
}
