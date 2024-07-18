import Array "mo:base/Array";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Hash "mo:base/Hash";
import Option "mo:base/Option";
import Text "mo:base/Text";

actor VotingPlatform {

  // Types
  type Proposal = {
    id : Nat;
    description : Text;
    votes : Nat;
  };

  type Vote = {
    proposalId : Nat;
    voter : Principal;
  };

  type Candidate = {
    id : Nat;
    name : Text;
    manifesto : Text;
  };

  type User = {
    principal : Principal;
    name : Text;
    isRegistered : Bool;
  };

  // Hash function for Nat
  func natHash(n : Nat) : Hash.Hash {
    Hash.hash(n)
  };

  // State variables
  private stable var nextProposalId : Nat = 0;
  private stable var nextCandidateId : Nat = 0;
  private var proposals = HashMap.HashMap<Nat, Proposal>(0, Nat.equal, natHash);
  private var votes = HashMap.HashMap<Principal, [Vote]>(0, Principal.equal, Principal.hash);
  private var candidates = HashMap.HashMap<Nat, Candidate>(0, Nat.equal, natHash);
  private var users = HashMap.HashMap<Principal, User>(0, Principal.equal, Principal.hash);

  // User registration
  public shared(msg) func registerUser(name : Text) : async Bool {
    let caller = msg.caller;
    switch (users.get(caller)) {
      case (?_) { false }; // User already registered
      case null {
        let newUser : User = {
          principal = caller;
          name = name;
          isRegistered = true;
        };
        users.put(caller, newUser);
        true
      };
    }
  };

  // Get user info
  public query func getUserInfo(userPrincipal : Principal) : async ?User {
    users.get(userPrincipal)
  };

  // Candidate registration
  public shared(msg) func registerCandidate(name : Text, manifesto : Text) : async ?Nat {
    let caller = msg.caller;
    switch (users.get(caller)) {
      case null { null }; // User not registered
      case (?user) {
        if (user.isRegistered) {
          let candidateId = nextCandidateId;
          let newCandidate : Candidate = {
            id = candidateId;
            name = name;
            manifesto = manifesto;
          };
          candidates.put(candidateId, newCandidate);
          nextCandidateId += 1;
          ?candidateId
        } else {
          null // User registered but not approved
        }
      };
    }
  };

  // Get candidate info
  public query func getCandidateInfo(candidateId : Nat) : async ?Candidate {
    candidates.get(candidateId)
  };

  // Get all candidates
  public query func getAllCandidates() : async [Candidate] {
    Iter.toArray(candidates.vals())
  };

  // Create a new proposal
  public shared(msg) func createProposal(description : Text) : async ?Nat {
    let caller = msg.caller;
    switch (users.get(caller)) {
      case null { null }; // User not registered
      case (?user) {
        if (user.isRegistered) {
          let proposalId = nextProposalId;
          let newProposal : Proposal = {
            id = proposalId;
            description = description;
            votes = 0;
          };
          proposals.put(proposalId, newProposal);
          nextProposalId += 1;
          ?proposalId
        } else {
          null // User registered but not approved
        }
      };
    }
  };

  // Cast a vote
  public shared(msg) func vote(proposalId : Nat) : async Bool {
    let caller = msg.caller;
    
    switch (users.get(caller)) {
      case null { false }; // User not registered
      case (?user) {
        if (not user.isRegistered) { return false }; // User not approved

        switch (proposals.get(proposalId)) {
          case null { false }; // Proposal doesn't exist
          case (?proposal) {
            // Check if the user has already voted for this proposal
            switch (votes.get(caller)) {
              case null { 
                votes.put(caller, [{proposalId = proposalId; voter = caller}]);
              };
              case (?userVotes) {
                if (Array.find(userVotes, func (v : Vote) : Bool { v.proposalId == proposalId }) != null) {
                  return false; // User has already voted for this proposal
                };
                votes.put(caller, Array.append(userVotes, [{proposalId = proposalId; voter = caller}]));
              };
            };
            
            // Update the proposal's vote count
            let updatedProposal : Proposal = {
              id = proposal.id;
              description = proposal.description;
              votes = proposal.votes + 1;
            };
            proposals.put(proposalId, updatedProposal);
            true
          };
        }
      };
    }
  };

  // Get all proposals
  public query func getAllProposals() : async [Proposal] {
    Iter.toArray(proposals.vals())
  };

  // Get a specific proposal
  public query func getProposal(proposalId : Nat) : async ?Proposal {
    proposals.get(proposalId)
  };

  // Get the number of votes for a specific proposal
  public query func getVotesForProposal(proposalId : Nat) : async Nat {
    switch (proposals.get(proposalId)) {
      case null { 0 };
      case (?proposal) { proposal.votes };
    }
  };

  // Get all votes cast by a specific user
  public query func getUserVotes(user : Principal) : async [Vote] {
    switch (votes.get(user)) {
      case null { [] };
      case (?userVotes) { userVotes };
    }
  };
}



