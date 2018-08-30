package turniej;

public class ReadingNumbersException extends Exception {
    private String reason;

    public ReadingNumbersException(String reason) {
        this.reason = reason;
    }

    public String getReason() {
        return reason;
    }
}
