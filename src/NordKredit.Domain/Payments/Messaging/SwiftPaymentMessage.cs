namespace NordKredit.Domain.Payments.Messaging;

/// <summary>
/// Payment clearing message for SWIFT international payments.
/// Maps to SWIFT MT103 (Single Customer Credit Transfer) format.
///
/// COBOL source: Payment programs using IBM MQ for SWIFT messaging.
/// Regulations: PSD2 Art. 97 (SCA), FFFS 2014:5 Ch. 8 (accurate records), AML/KYC screening.
/// SWIFT: Society for Worldwide Interbank Financial Telecommunication.
/// </summary>
public sealed record SwiftPaymentMessage
{
    /// <summary>
    /// Sender's reference — unique transaction reference (MT103 field 20).
    /// </summary>
    public required string SenderReference { get; init; }

    /// <summary>
    /// Bank operation code (MT103 field 23B). Typically "CRED" for credit transfer.
    /// </summary>
    public required string BankOperationCode { get; init; }

    /// <summary>
    /// Value date and currency/amount (MT103 field 32A — date).
    /// </summary>
    public required DateOnly ValueDate { get; init; }

    /// <summary>
    /// Currency code — ISO 4217 (MT103 field 32A — currency).
    /// </summary>
    public required string Currency { get; init; }

    /// <summary>
    /// Settlement amount (MT103 field 32A — amount).
    /// COBOL: PIC S9(13)V99 — decimal(15,2).
    /// </summary>
    public required decimal Amount { get; init; }

    /// <summary>
    /// Ordering customer name and address (MT103 field 50K — name).
    /// </summary>
    public required string OrderingCustomerName { get; init; }

    /// <summary>
    /// Ordering customer account (MT103 field 50K — account).
    /// IBAN format for EU transactions.
    /// </summary>
    public required string OrderingCustomerAccount { get; init; }

    /// <summary>
    /// Ordering institution BIC — sender bank (MT103 field 52A).
    /// NordKredit's SWIFT BIC code.
    /// </summary>
    public required string OrderingInstitutionBic { get; init; }

    /// <summary>
    /// Beneficiary customer name (MT103 field 59 — name).
    /// </summary>
    public required string BeneficiaryName { get; init; }

    /// <summary>
    /// Beneficiary customer account (MT103 field 59 — account).
    /// IBAN format for EU beneficiaries, local format otherwise.
    /// </summary>
    public required string BeneficiaryAccount { get; init; }

    /// <summary>
    /// Account with institution BIC — beneficiary bank (MT103 field 57A).
    /// </summary>
    public required string BeneficiaryInstitutionBic { get; init; }

    /// <summary>
    /// Remittance information — payment details (MT103 field 70).
    /// </summary>
    public string? RemittanceInformation { get; init; }

    /// <summary>
    /// Details of charges — SHA (shared), OUR (sender pays), BEN (beneficiary pays) (MT103 field 71A).
    /// </summary>
    public required string DetailsOfCharges { get; init; }

    /// <summary>
    /// Instructed amount in the original currency, if different from settlement (MT103 field 33B).
    /// </summary>
    public decimal? InstructedAmount { get; init; }

    /// <summary>
    /// Instructed currency, if different from settlement currency (MT103 field 33B).
    /// </summary>
    public string? InstructedCurrency { get; init; }
}
