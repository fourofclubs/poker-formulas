package ca.testing;

public class FML {
	private String message;
	
	public FML(String m){
		this.message = m;
	}
	
	public void print(String x){
		System.out.println(message + x);
		
	}
	public String getMessage(){
		return message;
	}
}
