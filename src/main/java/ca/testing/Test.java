package ca.testing;

public class Test {
	public static void main(String[] args) {
		FML n = new FML("Fuck my life");
		n.print(" Please");
		n.print(" Now!");

		FML n2 = new FML("Fuck off");
		n2.print(" Please.");

		godDammit(5);
		System.out.println();
		godDammit(2);
		
		if(n.getMessage().equals("Fuck my life"))System.out.println("Jesus Christ!");
		if(n.getMessage().equals("Jesus Christ!"))System.out.println("Please");
		if(n.getMessage().equals("Please"))System.out.println("Fuck off");

	}

	private static void godDammit(int n) {
		int b = 0;
		while (b < n) {
			System.out.println("god dammit!");
			b = b + 1;
		}
		
	}
	
}
